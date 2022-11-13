------------------------------------------------------------------------------
--                                                                          --
--                         JSON Parser/Constructor                          --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2021, ANNEXI-STRAYLINE Trans-Human Ltd.                   --
--  All rights reserved.                                                    --
--                                                                          --
--  Original Contributors:                                                  --
--  * Richard Wai (ANNEXI-STRAYLINE)                                        --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--                                                                          --
--      * Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--                                                                          --
--      * Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--                                                                          --
--      * Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     --
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       --
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A --
--  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT      --
--  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,   --
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT        --
--  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,   --
--  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY   --
--  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT     --
--  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE   --
--  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.    --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces;
with Ada.Strings.Wide_Wide_Fixed;

with AURA.JSON;

with Modular_Hashing.xxHash32;
with Modular_Hashing.xxHash64;

separate (JSON.Unbounded_Codecs)

package body Node_Hash_Maps is
   
   --
   -- Structural Components
   --
   
   package Config renames AURA.JSON.Configuration;
   use all type Config.Hash_Algorithm;
   
   type Hash_Container (Kind: Config.Hash_Algorithm) is
      record
         case Kind is
            when xxHash32 => XXH32: Modular_Hashing.xxHash32.XXH32_Hash;
            when xxHash64 => XXH64: Modular_Hashing.xxHash64.XXH64_Hash;
         end case;         
      end record;
   
   type Hash_Engine_Container (Kind: Config.Hash_Algorithm) is
      record
         case Kind is
            when xxHash32 => 
               XXH32: aliased Modular_Hashing.xxHash32.XXH32_Engine;
            when xxHash64 =>
               XXH64: aliased Modular_Hashing.xxHash64.XXH64_Engine;
         end case;
      end record;
   
   type Match_Slot (Hash_Kind: Config.Hash_Algorithm) is
      record
         Target   : Node    := null;
         Collision: Boolean := False;
         -- True if a full collision occured with the hash of this slot. This
         -- indicates to the look-up function that it needs to check the
         -- full name of the Target if it finds a matching hash
         
         Hash: Hash_Container (Config.Unbounded_Codec_Hash_Algo);
      end record;
   
   type Match_Set is array (Match_Level) of
     Match_Slot (Config.Unbounded_Codec_Hash_Algo);
   
   subtype Match_Table_Index is Interfaces.Unsigned_8;
   type Match_Table is array (Match_Table_Index) of Match_Set;
   
   type Node_Hash_Table is
      record
         Table        : Match_Table;
         
         Registrations: Natural := 0;
         -- This is the total number of Nodes that have been successfully
         -- registered on this table. This value is queried in order to
         -- detect when a Table is full. See Max_Registrations below.
         
         Next         : Node_Hash_Table_Access := null;
      end record;
   
   Max_Registrations: constant := Match_Table_Index'Modulus * Match_Level'Last;
   
   --
   -- Indexing Strategies
   --
   
   -- For each Table in the map's Table_Chain, one of two straties is used to
   -- index into the table depending on the Match Level. The strategies are
   -- alternated for each further table in the chain, beginning with the
   -- Primary Plan
   
   package Indexing_Strategies is
      
      type Indexing_Plan is array (Match_Level) of Match_Table_Index;
      
      function Compute_Primary_Plan (Hash: Hash_Container) 
                                    return Indexing_Plan;
      
      function Compute_Secondary_Plan (Hash: Hash_Container)
                                      return Indexing_Plan;
   
   end Indexing_Strategies;
   
   package body Indexing_Strategies is separate;
   
   
   -- Convenience Subprograms --
   
   generic
      type Identifier is private;
   procedure Generic_Hash_And_Plan 
     (ID            : in     Identifier;
      Hash          :    out Hash_Container;
      Primary_Plan  :    out Indexing_Strategies.Indexing_Plan;
      Secondary_Plan:    out Indexing_Strategies.Indexing_Plan);
   
   procedure Generic_Hash_And_Plan 
     (ID            : in     Identifier;
      Hash          :    out Hash_Container;
      Primary_Plan  :    out Indexing_Strategies.Indexing_Plan;
      Secondary_Plan:    out Indexing_Strategies.Indexing_Plan)
   is
      Algo: Config.Hash_Algorithm := Config.Unbounded_Codec_Hash_Algo;
      -- GNAT Bug work-around: We originally had this as a rename of
      -- Config.Unbounded_Coded_Hash_Algo. The idea was that it would
      -- allow the compiler to optimize the entire invocation of the
      -- following case statement. However making Algo constant causes
      -- GNAT (11.3.0) to flip-out entirely, causing all sorts of false-
      -- positive errors even outside of this package.
      --
      -- Basically it seems GNAT 11.3.0 is smart enough to understand that
      -- creating a discriminated object where the discriminent is set
      -- via a library-level constant means that it can know some things
      -- "for sure" about that object. But it isn't smart enough to know if
      -- logic, such as the above switch statement has actually checked it or
      -- not.
      --
      -- What should be happening is that GNAT should be using similar smarts
      -- to recognize that entire branches of logic will never happen
      -- (dead code elimination), and do that elimnation before it sees if that
      -- code is going to raise an exception at run-time, or assumes that we
      -- are "definately" accessing something that is not there
      --
      -- /rant
      
      Engine: Hash_Engine_Container (Algo);
   begin
      -- Compute the Hash
      case Algo is
         when xxHash32 =>
            Identifier'Write (Engine.XXH32'Access, ID);
            Hash.XXH32 := Modular_Hashing.xxHash32.XXH32_Hash 
              (Engine.XXH32.Digest);
            
         when xxHash64 => 
            Identifier'Write (Engine.XXH64'Access, ID);
            Hash.XXH64 := Modular_Hashing.xxHash64.XXH64_Hash 
              (Engine.XXH64.Digest);
      end case;
      
      -- Compute the plans
      Primary_Plan   := Indexing_Strategies.Compute_Primary_Plan   (Hash);
      Secondary_Plan := Indexing_Strategies.Compute_Secondary_Plan (Hash);
      
   end Generic_Hash_And_Plan;
                           
   
   
   --
   -- Implementation 
   --
   
   -----------------------
   -- Compute_Full_Path --
   -----------------------
   
   -- Computes the full javascript-style path to Node To_Node, for use in the
   -- Codec's Path_Map. Path is Cleared first
   
   procedure Compute_Full_Path
     (To_Node: in     not null Node;
      Path   : in out Slab_Strings.Slab_String)
   is
      use Slab_Strings;
      -- Since Codecs are not task-safe, we have a special Slab_String buffer
      -- in the codec that we can use for purposes such as this
      
      
      function Array_Index_Path_Component (Index: Natural) 
                                          return JSON_String_Value
      is
         function Trim (Source: in Wide_Wide_String;
                        Side  : in Ada.Strings.Trim_End := Ada.Strings.Left)
                       return Wide_Wide_String 
           renames Ada.Strings.Wide_Wide_Fixed.Trim;
      begin
         return '[' & Trim (Natural'Wide_Wide_Image (Index)) & ']';
      end;
      
      
      procedure Recursive_Add_Parent (Child: not null Node);
      
      procedure Recursive_Add_Parent (Child: not null Node) is
      begin
         if Child.Parent /= null then
            Recursive_Add_Parent (Child.Parent);
         else
            -- This is literally the root, which is nameless and thus is the
            -- end of the recursion. Now was we pop back, we'll append each
            -- name in order
            return;
         end if;
         
         case JSON_Structure_Kind (Child.Parent.Kind) is
            when JSON_Object =>
               if Child.Parent.Parent /= null then
                  Append (Path, ".");
                  -- Don't do this for any of the first children of root
               end if;
               
               Append (Path, To_JSON_String (Child.Name));
               
            when JSON_Array =>
               Append (Path, Array_Index_Path_Component (Child.Index));
         end case;
      end;
   begin
      Clear (Path);
      Recursive_Add_Parent (To_Node);
   end Compute_Full_Path;
   
   
   -----------
   -- Setup --
   -----------
   
   procedure Setup (Map    : in out Node_Hash_Map;
                    Subpool: in     not null Subpool_Handle)
   is begin
      Map.Subpool := Subpool;
      Map.Expected_ID := new (Subpool) Slab_Strings.Slab_String;
      Map.Candidate_ID := new (Subpool) Slab_Strings.Slab_String;
      
      Slab_Strings.Setup (Target  => Map.Expected_ID.all,
                          Subpool => Subpool);
      Slab_Strings.Setup (Target  => Map.Candidate_ID.all,
                          Subpool => Subpool);
   end;
   
   ------------------
   -- Registration --
   ------------------
   
   generic
      type Identifier is private;
      
      with function Same_Identity (Candidate: not null Node; 
                                   Test     : Identifier)
                                  return Boolean;
   
   -- Shall return True if Expected is truly the Identifier for the
   -- Candidate Node. This is called if a full collision occurs, and if
   -- True is returned, a double-registration has occured, and
   -- Constriant_Error is raised
   
   procedure Generic_Register (Map       : in out Node_Hash_Map;
                               Registrant: in     not null Node; 
                               Node_ID   : in     Identifier);
   
   procedure Generic_Register (Map       : in out Node_Hash_Map;
                               Registrant: in     not null Node; 
                               Node_ID   : in     Identifier)
   is
      use Indexing_Strategies;
      
      procedure Hash_And_Plan is new Generic_Hash_And_Plan (Identifier);
      
      Algo : Config.Hash_Algorithm renames Config.Unbounded_Codec_Hash_Algo;
      
      Hash : Hash_Container (Algo);
      
      Active_Table  : Node_Hash_Table_Access;
      Table_Sequence: Positive;
      
      type Selected_Plan is access all Indexing_Plan;
      Primary_Plan, Secondary_Plan: aliased Indexing_Plan;
      Active_Plan: Selected_Plan;
      
      function Try_Register return Boolean with Inline is
         -- Attempt to register the Node with the current Active_Table and
         -- Active_Plan. Return True if successful
      begin
         for Level in Match_Level loop
            declare
               Candidate: Match_Slot 
                 renames Active_Table.Table(Active_Plan.all(Level))(Level);
            begin
               
               if Candidate.Target = null then
                  -- Found a spot!
                  Candidate.Target := Registrant;
                  Candidate.Hash   := Hash;
                  
                  Active_Table.Registrations := Active_Table.Registrations + 1;
                  
                  if Table_Sequence = 1 then
                     Map.Profile.Primary_Registrations(Level) 
                       := Map.Profile.Primary_Registrations(Level) + 1;
                  else
                     Map.Profile.Overflow_Registrations(Level) 
                       := Map.Profile.Overflow_Registrations(Level) + 1;
                  end if;
                  
                  return True;
               end if;
               
               -- If we're still going, the spot is occupied
               -- Check for a collision, and mark if we find it, mark it
               -- as such. Also check if there is a full collision here, then
               -- check the Identity of the existing Node to ensure that it is
               -- not the same
               
               if Candidate.Hash = Hash then
                  if Same_Identity (Candidate => Candidate.Target, 
                                    Test      => Node_ID) 
                  then
                     raise Constraint_Error with
                       "Double registration of node identifier.";
                     
                  elsif Candidate.Collision then
                     -- So the actual Identifier does not match the existing
                     -- registrant, but the hash is exactly the same
                     -- (a collision!) so we make note of that fact, so that
                     -- any subsequent lookup knows that it needs to actually
                     -- check the identity, not just the hash, and may need
                     -- to continue looking
                     
                     Candidate.Collision := True;
                     Map.Profile.Full_Collisions
                       := Map.Profile.Full_Collisions + 1;
                  end if;
               end if;
               
               -- If we get here, there was no room for the node, so we
               -- continue-on with the next tactic
               
            end;
         end loop;
         
         return False;
      end Try_Register;
      
   begin
      Hash_And_Plan (Node_ID, Hash, Primary_Plan, Secondary_Plan);
      
      -- Allocate the map's first table if needed
      if Map.Table_Chain = null then
         Map.Table_Chain := new (Map.Subpool) Node_Hash_Table;
         Map.First_Unsaturated := Map.Table_Chain;
         Map.Unsaturated_Sequence := 1;
      end if;
      -- Find the best place to start - i.e. the first table known not
      -- to be saturated
      
      Active_Table := Map.First_Unsaturated;
      Table_Sequence := Map.Unsaturated_Sequence;      
      
      -- Now first check if Active_Table is "still" unsaturated. The last
      -- registration to make it saturated doesn't actually advance this, since
      -- doing it this way alleviates us of having to check this on every
      -- iteration of the main loop
      --
      -- Note that it is possible that the last registration "closed" a gap
      -- of saturated tables, thus we put a loop here to catch that when it
      -- happens
      
      while Active_Table.Registrations = Max_Registrations  loop
         if Active_Table.Next = null then
            Active_Table.Next  := new (Map.Subpool) Node_Hash_Table;
            Map.Profile.Total_Tables := Map.Profile.Total_Tables + 1;
         end if;
         
         Active_Table := Active_Table.Next;
         Table_Sequence := Table_Sequence + 1;
         Map.First_Unsaturated := Active_Table;
         Map.Unsaturated_Sequence := Table_Sequence;
      end loop;
      
      -- Log the performance
      Map.Profile.Saturation_High_Water := Map.Unsaturated_Sequence - 1;
      
      -- Go hunting for an empty spot to park Registrant.
      
      loop
         Active_Plan := (if Table_Sequence mod 2 = 0 then 
                            Secondary_Plan'Access 
                         else 
                            Primary_Plan'Access);
         
         -- For odd table sequences, use the Primary_Plan, for even sequences,
         -- use the Secondary_Plan

         exit when Try_Register;
         
         -- No room in this table, move onto the next one, or allocate a new
         -- table
         
         if Active_Table.Next = null then
            Active_Table.Next := new (Map.Subpool) Node_Hash_Table;
            Map.Profile.Total_Tables := Map.Profile.Total_Tables + 1;
         end if;
         
         Table_Sequence := Table_Sequence + 1;
         Active_Table := Active_Table.Next;

      end loop;
      
   end Generic_Register;
   
   ----------------------------------------------------------------------
   
   procedure Register (Path_Map  : in out Node_Hash_Map; 
                       Registrant: in     not null Node)
   is
      use type Slab_Strings.Slab_String;
      
      
      function Same_Path (Candidate: not null Node;
                          Test     : Slab_Strings.Slab_String)
                         return Boolean
      is begin
         Compute_Full_Path (To_Node => Candidate,
                            Path    => Path_Map.Candidate_ID.all);
         
         return Path_Map.Candidate_ID.all = Test;
      end;
      
      
      function Same_Name (Candidate: not null Node; 
                          Test     : Slab_Strings.Slab_String)
                         return Boolean
      is begin
         return Candidate.Name = Test;
      end;
      
      
      function Same_Index (Candidate: not null Node; 
                           Test     : Natural)
                          return Boolean
      is begin
         return Candidate.Index = Test;
      end;
      
      
      procedure Register_Path is new Generic_Register
        (Identifier    => Slab_Strings.Slab_String,
         Same_Identity => Same_Path);
      
      procedure Register_Name is new Generic_Register 
        (Identifier    => Slab_Strings.Slab_String,
         Same_Identity => Same_Name);
      
      procedure Register_Index is new Generic_Register 
        (Identifier    => Natural,
         Same_Identity => Same_Index);

   begin
      Compute_Full_Path (To_Node => Registrant, 
                         Path    => Path_Map.Expected_ID.all);
      
      Register_Path (Map        => Path_Map,
                     Registrant => Registrant,
                     Node_ID    => Path_Map.Expected_ID.all);
      
      -- Note that Parent is never null during a Regisration since the
      -- only node without a Parent is Root, which is never Registered
      
      case JSON_Structure_Kind (Registrant.Parent.Kind) is
         when JSON_Object =>
            Register_Name (Map        => Registrant.Parent.Container.Name_Map,
                           Registrant => Registrant,
                           Node_ID    => Registrant.Name);
         when JSON_Array =>
            Register_Index (Map        => Registrant.Parent.Container.Index_Map,
                            Registrant => Registrant,
                            Node_ID    => Registrant.Index);
      end case;
   end Register;
   
   
   --------------------
   -- Generic_Lookup --
   --------------------
   
   generic
      type Identifier is private;
      
      with function Same_Identity (Candidate: not null Node; 
                                   Test     : Identifier)
                                  return Boolean;
   
   function Generic_Lookup (Map: Node_Hash_Map; ID: Identifier)
                           return Node;
   
   function Generic_Lookup (Map: Node_Hash_Map; ID: Identifier)
                           return Node
   is 
      use Indexing_Strategies;
      
      procedure Hash_And_Plan is new Generic_Hash_And_Plan (Identifier);
      
      Algo : Config.Hash_Algorithm renames Config.Unbounded_Codec_Hash_Algo;
      
      Hash : Hash_Container (Algo);
      
      Active_Table: Node_Hash_Table_Access := Map.Table_Chain;
      
      type Selected_Plan is access all Indexing_Plan;
      Primary_Plan, Secondary_Plan: aliased Indexing_Plan;
      Use_Primary: Boolean := True;
      Active_Plan: Selected_Plan;
   begin
      Hash_And_Plan (ID, Hash, Primary_Plan, Secondary_Plan);
      
      -- Go hunting for a match
      while Active_Table /= null loop
         Active_Plan := (if Use_Primary then Primary_Plan'Access 
                         else Secondary_Plan'Access);
         
         for Level in Match_Level loop
            declare
               Candidate: Match_Slot 
                 renames Active_Table.Table(Active_Plan.all(Level))(Level);
            begin
               if Candidate.Target /= null
                 and then Candidate.Hash = Hash
               then
                  -- Possible match
                  if Same_Identity (Candidate.Target, ID) then
                     -- Bingo
                     return Candidate.Target;
                     
                  elsif not Candidate.Collision then
                     -- Now this is where we get to save some time. So the hash
                     -- is a perfect match, but the actual identifer is not a
                     -- match. If there is no collosion registered for this
                     -- slot, we are safe to assume that the identifier has not
                     -- be registered at all
                     return null;
                     
                  end if;
               end if;
            end;
         end loop;
         
         -- Alternate the plan
         Use_Primary := not Use_Primary;
         
         Active_Table := Active_Table.Next;
      end loop;
      
      return null;
      
   end Generic_Lookup;
   
   ------------
   -- Lookup --
   ------------
   
   function Lookup_By_Path (Path_Map: Node_Hash_Map;
                            Path    : JSON_String_Value)
                           return Node
   is
      use Slab_Strings;
      
      function Same_Path (Candidate: not null Node;
                          Test     : Slab_String)
                         return Boolean
      is begin
         Compute_Full_Path (To_Node => Candidate,
                            Path    => Path_Map.Candidate_ID.all);
         
         return Path_Map.Candidate_ID.all = Test;
      end;
      
      
      function Lookup_Actual is new Generic_Lookup
        (Identifier    => Slab_String,
         Same_Identity => Same_Path);
   begin
      Clear (Path_Map.Expected_ID.all);
      Append (Path_Map.Expected_ID.all, Path);
      return Lookup_Actual (Map => Path_Map, ID => Path_Map.Expected_ID.all);
   end Lookup_By_Path;
   
   ----------------------------------------------------------------------
   
   function  Lookup_By_Name (Name_Map : Node_Hash_Map; Name: JSON_String_Value)
                            return Node
   is 
      use Slab_Strings;
      
      function Same_Name (Candidate: not null Node; 
                          Test     : Slab_String)
                         return Boolean
      is begin
         return Candidate.Name = Test;
      end;
      
      function Lookup_Actual is new Generic_Lookup
        (Identifier    => Slab_String,
         Same_Identity => Same_Name);
   begin
      Clear (Name_Map.Expected_ID.all);
      Append (Name_Map.Expected_ID.all, Name);
      return Lookup_Actual (Map => Name_Map, ID => Name_Map.Expected_ID.all);
   end Lookup_By_Name;
   
   ----------------------------------------------------------------------
   
   function  Lookup_By_Index (Index_Map: Node_Hash_Map; Index: Natural)
                             return Node
   is 
      function Same_Index (Candidate: not null Node; 
                           Test     : Natural)
                          return Boolean
      is (Candidate.Index = Test);
      
      function Lookup_Actual is new Generic_Lookup
        (Identifier => Natural, Same_Identity => Same_Index);
   begin
      return Lookup_Actual (Map => Index_Map, ID => Index);
   end Lookup_by_Index;
   
   -----------------
   -- Performance --
   -----------------
   
   function Performance (Map: Node_Hash_Map) return Performance_Counters is
     (Map.Profile);
   
end Node_Hash_Maps;
