------------------------------------------------------------------------------
--                                                                          --
--                         JSON Parser/Constructor                          --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2021-2022, ANNEXI-STRAYLINE Trans-Human Ltd.              --
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

with Ada.Real_Time;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Maps;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Characters.Conversions;
with Ada.Unchecked_Deallocate_Subpool;

with Unicode.UTF8_Stream_Decoder;

with JSON.Standards;

package body JSON.Unbounded_Codecs is
   
   function To_Wide_Wide_String (Item: in String) return Wide_Wide_String
     renames Ada.Characters.Conversions.To_Wide_Wide_String;
   
   --
   -- Tools
   --
   
   -----------------
   -- Append_Node --
   -----------------
   
   -- Allocates and initializes a new node that is appended to Parent as a
   -- child, and returns the reference. 
   --
   -- This process properly initializes the Index, as well as updating the
   -- Member/Element counter of the Parent.
   --
   -- The container is default initialized (JSON_Null), and the callee is
   -- reponsible for mutating the Container, and properly initializing it.
   
   function Append_Node (Parent: not null Node) return not null Node
   is
      New_Node: Node := new (Parent.Codec_Subpool) JSON_Value;
   begin
      New_Node.Root := (if Parent.Parent = null then Parent else Parent.Root);
      New_Node.Parent := Parent;
      New_Node.Codec_Subpool := Parent.Codec_Subpool;
      
      case JSON_Structure_Kind (Parent.Container.Kind) is
         when JSON_Object =>
            if Parent.Container.First_Member = null then
               pragma Assert (Parent.Container.Last_Member = null);
               Parent.Container.First_Member := New_Node;
               Parent.Container.Last_Member  := New_Node;
               New_Node.Index := 0;
            else
               pragma Assert (Parent.Container.Last_Member.Next = null);
               Parent.Container.Last_Member.Next := New_Node;
               New_Node.Prev := Parent.Container.Last_Member;
               New_Node.Index := Parent.Container.Last_Member.Index + 1;
               Parent.Container.Last_Member := New_Node;
            end if;
            
            Slab_Strings.Setup (Target  => New_Node.Name,
                                Subpool => New_Node.Codec_Subpool);
            New_Node.Index := Parent.Container.Member_Count;
            Parent.Container.Member_Count := Parent.Container.Member_Count + 1;

         when JSON_Array =>
            if Parent.Container.First_Element = null then
               pragma Assert (Parent.Container.Last_Element = null);
               Parent.Container.First_Element := New_Node;
               Parent.Container.Last_Element  := New_Node;
               New_Node.Index := 0;
            else
               pragma Assert (Parent.Container.Last_Element.Next = null);
               Parent.Container.Last_Element.Next := New_Node;
               New_Node.Prev := Parent.Container.Last_Element;
               New_Node.Index := Parent.Container.Last_Element.Index + 1;
               Parent.Container.Last_Element := New_Node;
            end if;
            
            New_Node.Index := Parent.Container.Element_Count;
            Parent.Container.Element_Count 
              := Parent.Container.Element_Count + 1;
      end case;
      
      return New_Node;
   end Append_Node;
   
   
   ---------------
   -- Seek_Node --
   ---------------
   
   -- Seek Node obtains the Node value of a an actual JSON_Value within a Codec.
   -- This is primarily used by Codec.Delve and Codec.Constant_Delve to
   -- generate the JSON_Mutable_Structure, which contains a reference, without
   -- resorting to Unchecked_Access.
   --
   -- Seek_Node also ensures Value belongs to Codec, and raises Program_Error
   -- if it does not.
   
   function Seek_Node (Codec: Unbounded_JSON_Codec;
                       Value: JSON_Value'Class)
                      return not null Node with Inline
   is begin
      if Value.Root /= Codec.Root_Node then
         raise Program_Error with "Value is not a member of Codec";
      
      elsif Value.Parent = null then
         return Codec.Root_Node;
      elsif Value.Prev /= null then
         return Value.Prev.Next;
      elsif Value.Next /= null then
         return Value.Next.Prev;
      else
         -- This should not possibly fail
         case JSON_Structure_Kind (Value.Parent.Container.Kind) is
            when JSON_Object =>
               pragma Assert 
                 (Value.Parent.Container.Member_Count = 1);
               return Value.Parent.Container.First_Member;
            when JSON_Array =>
               pragma Assert 
                 (Value.Parent.Container.Element_Count = 1);
               return Value.Parent.Container.First_Element;
         end case;
      end if;
   end Seek_Node;
   
   --
   -- Internal Subsystem Implementations
   --
   
   package body Slab_Strings         is separate;
   package body Node_Hash_Maps       is separate;
   
   
   --
   -- JSON_Value
   --
   
   ----------
   -- Kind --
   ----------
   
   function Kind (Value: JSON_Value) return JSON_Value_Kind is
     (Value.Container.Kind);
   
   -----------------
   -- Parent_Kind --
   -----------------
   
   function Parent_Kind (Value: JSON_Value) return JSON_Structure_Kind is
   begin
      if Value.Parent = null then
         -- This indicates the root object
         raise Constraint_Error with
              "The Codec Root object has no parent.";
      else
         return Value.Parent.Container.Kind;
      end if;
   end;
   
   ----------
   -- Name --
   ----------
   
   function Name (Value: JSON_Value) return JSON_String_Value is
   begin
      -- Explicit check for the precondition, since this could be a common
      -- mistake
      
      if Value.Container.Kind /= JSON_Object then
         raise Constraint_Error with "Value is not named. Only members of "
           & "JSON objects are named.";
      end if;
      
      return Slab_Strings.To_JSON_String (Value.Name);
   end;
   
   ---------------
   -- UTF8_Name --
   ---------------
   
   function UTF8_Name (Value: JSON_Value) return UTF_8_String is
      use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
      
      Wide_Wide_Name: constant JSON_String_Value 
        := Slab_Strings.To_JSON_String (Value.Name);
   begin
      return Encode (Wide_Wide_Name);
   end;

   -----------
   -- Index --
   -----------
   
   function Index (Value: JSON_Value) return Natural is (Value.Index);

   -----------
   -- Value -- (Get)
   -----------
   
   function Value (Value: JSON_Value) return Boolean is
     (Value.Container.Boolean_Value);
   
   function Value (Value: JSON_Value) return JSON_Integer_Value is
     (Value.Container.Integer_Value);
      
   function Value (Value: JSON_Value) return JSON_Float_Value is
     (Value.Container.Float_Value);
   
   function Value (Value: JSON_Value) return JSON_String_Value is
     (Slab_Strings.To_JSON_String (Value.Container.String_Value));
   
   function UTF8_Value (Value: JSON_Value) return UTF_8_String is
     (Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (Value.Value));
   
   -----------
   -- Value -- (Set)
   -----------
   
   procedure Value (Target: in out JSON_Value;
                    Value : in     Boolean;
                    Mutate: in     Boolean := False)
   is begin
      if Target.Container.Kind = JSON_Boolean then
         Target.Container.Boolean_Value := Value;
      elsif Mutate or else Target.Container.Kind = JSON_Null then
         -- Full mutation
         Target.Container 
           := Value_Container'(Kind          => JSON_Boolean,
                               Boolean_Value => Value);

      else
         raise Constraint_Error with 
           "Intended value (JSON_INTEGER) does not fit the Kind of the target "
           & '(' & JSON_Value_Kind'Image (Target.Container.Kind) & ").";
      end if;
   end;
   
   ----------------------------------------------------------------------
   
   procedure Value (Target: in out JSON_Value; 
                    Value : in     JSON_Integer_Value;
                    Mutate: in     Boolean := False)
   is begin
      if Target.Container.Kind = JSON_Integer then
         Target.Container.Integer_Value := Value;
      elsif Mutate or else Target.Container.Kind = JSON_Null then
         -- Full mutation
         Target.Container 
           := Value_Container'(Kind          => JSON_Integer,
                               Integer_Value => Value);
      else
         raise Constraint_Error with 
           "Intended value (JSON_INTEGER) does not fit the Kind of the target "
           & '(' & JSON_Value_Kind'Image (Target.Container.Kind) & ").";
      end if;
   end;
   
   ----------------------------------------------------------------------
   
   procedure Value (Target: in out JSON_Value;
                    Value : in     JSON_Float_Value;
                    Mutate: in     Boolean := False) 
   is begin
      if Target.Container.Kind = JSON_Float then
         Target.Container.Float_Value := Value;
      elsif Mutate or else Target.Kind = JSON_Null then
         -- Full mutation
         Target.Container 
           := Value_Container'(Kind        => JSON_Float,
                               Float_Value => Value);
      else
         raise Constraint_Error with 
           "Intended value (JSON_FLOAT) does not fit the Kind of the target "
           & '(' & JSON_Value_Kind'Image (Target.Container.Kind) & ").";
      end if;
   end;
   
   ----------------------------------------------------------------------
   
   procedure Value (Target: in out JSON_Value;
                    Value : in     JSON_String_Value;
                    Mutate: in     Boolean := False)
   is
      procedure Set_Value with Inline is
         use Slab_Strings;
      begin
         Clear  (Target.Container.String_Value);
         Append (Target.Container.String_Value, Value);
      end;
   begin
      if Target.Container.Kind = JSON_String then
         Set_Value;
      elsif Mutate or else Target.Container.Kind = JSON_Null then
         -- Full mutation
         Target.Container := Value_Container'(Kind   => JSON_String,
                                              others => <>);
         Slab_Strings.Setup (Target  => Target.Container.String_Value,
                             Subpool => Target.Codec_Subpool);
         Set_Value;
      else
         raise Constraint_Error with 
           "Intended value (JSON_STRIJNG) does not fit the Kind of the target "
           & '(' & JSON_Value_Kind'Image (Target.Container.Kind) & ").";
      end if;
   end;
   
   -------------
   -- Nullify --
   -------------
   
   procedure Nullify (Target: in out JSON_Value) is
   begin
      Target.Container := Value_Container'(Kind => JSON_Null);
   end;
   
   --
   -- JSON_Constant_Structure / JSON_Mutable_Structure
   --
   
   ----------
   -- Kind --
   ----------
   
   function Kind (Structure: JSON_Constant_Structure) return
     JSON_Structure_Kind is (Structure.Structure_Root.Container.Kind);
   
   function Kind (Structure: JSON_Mutable_Structure) return
     JSON_Structure_Kind is (Structure.Structure_Root.Container.Kind);
   
   ------------
   -- Length --
   ------------
   
   function Length (Structure: JSON_Constant_Structure) return Natural is
      Struct_Actual: JSON_Value renames Structure.Structure_Root.all;
   begin
      case JSON_Structure_Kind (Struct_Actual.Container.Kind) is
         when JSON_Object => return Struct_Actual.Container.Member_Count;
         when JSON_Array  => return Struct_Actual.Container.Element_Count;
      end case;
   end;
   
   ----------------------------------------------------------------------
   
   function Length (Structure: JSON_Mutable_Structure) return Natural is
     (JSON_Constant_Structure'(Structure_Root => Structure.Structure_Root,
                               Codec_Constant => Structure.Codec_Constant)
          .Length);
   
   
   ----------------
   -- Has_Member --
   ----------------
   
   function Has_Member (Structure: JSON_Constant_Structure;
                        Name     : JSON_String_Value)
                       return Boolean
   is
      use Node_Hash_Maps;
      Struct_Actual: JSON_Value renames Structure.Structure_Root.all;
   begin
      if Structure.Codec_Constant.Write_Only then
         raise Constraint_Error with "Cannot index. Codec is write-only.";
      elsif Struct_Actual.Container.Kind /= JSON_Object then
         return False;
      else
         return Lookup_By_Name (Name_Map => Struct_Actual.Container.Name_Map,
                                Name     => Name) 
           /= null;
      end if;
   end;
   
   ----------------------------------------------------------------------
   
   function Has_Member (Structure: JSON_Mutable_Structure;
                        Name     : JSON_String_Value)
                       return Boolean
   is (JSON_Constant_Structure'(Structure_Root => Structure.Structure_Root,
                                Codec_Constant => Structure.Codec_Constant)
           .Has_Member(Name));
   
   --------------------
   -- Array Indexing --
   --------------------
   
   function JCS_Reference_Node (Structure: JSON_Constant_Structure;
                                Index    : JSON_Array_Index)
                               return not null Node
   is
      Struct_Actual: JSON_Value renames Structure.Structure_Root.all;
   begin
      if Structure.Codec_Constant.Write_Only then
         raise Constraint_Error with "Cannot index. Codec is write-only.";
         
      elsif Struct_Actual.Container.Kind /= JSON_Array then
         -- This allows a smart compiler to eliminate remaining discrimint
         -- checks
         raise Constraint_Error with
           "Integer indexing only applies to JSON_Array types";
         
      elsif Index >= Struct_Actual.Container.Element_Count then
         raise Constraint_Error with "Index is out of range of JSON_Array";
         
      end if;
      
      return Node_Hash_Maps.Lookup_By_Index 
        (Index_Map => Struct_Actual.Container.Index_Map,
         Index     => Index);
      
      -- Null check is enforced here if, somehow, Index is not valid
   end JCS_Reference_Node;
   
   ----------------------------------------------------------------------
   
   function JCS_Constant_Reference
     (Structure: JSON_Constant_Structure; Index: JSON_Array_Index)
     return JSON_Value_Constant_Reference
   is ((Ref => JCS_Reference_Node (Structure, Index)));
   
   ----------------------------------------------------------------------
   
   function JMS_Constant_Reference
     (Structure: JSON_Mutable_Structure; Index: JSON_Array_Index)
     return JSON_Value_Constant_Reference
   is (JSON_Constant_Structure'(Structure_Root => Structure.Structure_Root,
                                Codec_Constant => Structure.Codec_Constant)
           .JCS_Constant_Reference (Index));
   
   ----------------------------------------------------------------------
   
   function JMS_Reference (Structure: in out JSON_Mutable_Structure;
                           Index    : in     JSON_Array_Index)
                          return JSON_Value_Reference
   is
      Constant_Structure: JSON_Constant_Structure := 
        (Structure_Root => Structure.Structure_Root,
         Codec_Constant => Structure.Codec_Constant);
      
      Target: Node := JCS_Reference_Node (Structure => Constant_Structure,
                                          Index     => Index);
   begin
      return (Ref => Target);
   end;
   
   ----------------------------
   -- Object (Name) Indexing --
   ----------------------------
   
   function JCS_Reference_Node (Structure: JSON_Constant_Structure;
                                Name     : JSON_String_Value)
                               return not null Node
   is
      Struct_Actual: JSON_Value renames Structure.Structure_Root.all;
      Hit: Node;
   begin
      if Structure.Codec_Constant.Write_Only then
         raise Constraint_Error with "Cannot index. Codec is write-only.";
      elsif Struct_Actual.Container.Kind /= JSON_Object then
         raise Constraint_Error with
           "Name indexing only applies to JSON_Object types";
      end if;
      
      Hit := Node_Hash_Maps.Lookup_By_Name
        (Name_Map => Struct_Actual.Container.Name_Map,
         Name     => Name);
      
      if Hit = null then
         raise Constraint_Error with "Object does not contain named member.";
      else
         return Hit;
      end if;
   end JCS_Reference_Node;
   
   ----------------------------------------------------------------------
   
   function JCS_Constant_Reference
     (Structure: JSON_Constant_Structure; Name: JSON_String_Value)
     return JSON_Value_Constant_Reference
   is ((Ref => JCS_Reference_Node (Structure, Name)));
   
   ----------------------------------------------------------------------
   
   function JMS_Constant_Reference
     (Structure: JSON_Mutable_Structure; Name: JSON_String_Value)
     return JSON_Value_Constant_Reference
   is (JSON_Constant_Structure'(Structure_Root => Structure.Structure_Root,
                                Codec_Constant => Structure.Codec_Constant)
           .JCS_Constant_Reference (Name));
   
   ----------------------------------------------------------------------
   
   function JMS_Reference (Structure: in out JSON_Mutable_Structure;
                           Name     : in     JSON_String_Value)
                          return JSON_Value_Reference
   is
      Constant_Structure: JSON_Constant_Structure := 
        (Structure_Root => Structure.Structure_Root,
         Codec_Constant => Structure.Codec_Constant);
      
      Target: Node := JCS_Reference_Node (Structure => Constant_Structure,
                                          Name      => Name);
   begin
      return (Ref => Target);
   end;
   
   ---------------
   -- Has_Value --
   ---------------
   
   function Has_Value (Position: JSON_Structure_Cursor) return Boolean is
     (Position.Target /= null);   
   
   ---------------------
   -- Cursor Indexing --
   ---------------------
   
   function JCS_Reference_Node (Structure: JSON_Constant_Structure;
                                Position : JSON_Structure_Cursor)
                               return not null Node
   is begin
      if Position.Target = null then
         raise Constraint_Error with "Cursor does not designate a value.";
      elsif Position.Target.Parent /= Structure.Structure_Root then
         raise Constraint_Error with "Cursor is not from this structure.";
      else
         return Position.Target;
      end if;
   end JCS_Reference_Node;
   
   ----------------------------------------------------------------------
   
   function JCS_Constant_Reference (Structure: JSON_Constant_Structure;
                                    Position : JSON_Structure_Cursor)
                                   return JSON_Value_Constant_Reference
   is ((Ref => JCS_Reference_Node (Structure, Position)));
   
   ----------------------------------------------------------------------
   
   function JMS_Constant_Reference (Structure: JSON_Mutable_Structure;
                                    Position : JSON_Structure_Cursor)
                                   return JSON_Value_Constant_Reference
   is (JSON_Constant_Structure'(Structure_Root => Structure.Structure_Root,
                                Codec_Constant => Structure.Codec_Constant)
           .JCS_Constant_Reference (Position));
   
   ----------------------------------------------------------------------
   
   function JMS_Reference (Structure: in out JSON_Mutable_Structure;
                           Position : in     JSON_Structure_Cursor)
                          return JSON_Value_Reference
   is
      Constant_Structure: JSON_Constant_Structure := 
        (Structure_Root => Structure.Structure_Root,
         Codec_Constant => Structure.Codec_Constant);
      
      Target: Node := JCS_Reference_Node (Structure => Constant_Structure,
                                          Position  => Position);
   begin
      return (Ref => Target);
   end;
   
   
   ---------------
   -- Iteration --
   ---------------
   
   type Structure_Iterator is limited 
     new JSON_Structure_Iterators.Reversible_Iterator with
      record
         Structure_Root: not null Node;
      end record;
   
   overriding 
   function First (Object: Structure_Iterator) return JSON_Structure_Cursor;
   
   overriding
   function Next (Object: Structure_Iterator; Position: JSON_Structure_Cursor) 
                 return JSON_Structure_Cursor;
   
   overriding
   function Last (Object: Structure_Iterator) return JSON_Structure_Cursor;
   
   overriding
   function Previous (Object  : Structure_Iterator; 
                      Position: JSON_Structure_Cursor) 
                     return JSON_Structure_Cursor;
   
   ----------------------------------------------------------------------
   
   function JCS_Iterate
     (Structure: JSON_Constant_Structure)
     return JSON_Structure_Iterators.Reversible_Iterator'Class
   is (Structure_Iterator'(Structure_Root => Structure.Structure_Root));
   
   function JMS_Iterate
     (Structure: JSON_Mutable_Structure)
     return JSON_Structure_Iterators.Reversible_Iterator'Class
   is (Structure_Iterator'(Structure_Root => Structure.Structure_Root));
   
   ----------------------------------------------------------------------
   
   function First (Object: Structure_Iterator) return JSON_Structure_Cursor is
      Struct_Actual: JSON_Value renames Object.Structure_Root.all;
   begin
      case JSON_Structure_Kind (Struct_Actual.Container.Kind) is
         when JSON_Object => 
            return (Target => Struct_Actual.Container.First_Member);
         when JSON_Array  => 
            return (Target => Struct_Actual.Container.First_Element);
      end case;
   end;
   
   ----------------------------------------------------------------------
   
   function Last (Object: Structure_Iterator) return JSON_Structure_Cursor is
      Struct_Actual: JSON_Value renames Object.Structure_Root.all;
   begin
      case JSON_Structure_Kind (Struct_Actual.Container.Kind) is
         when JSON_Object => 
            return (Target => Struct_Actual.Container.Last_Member);
         when JSON_Array =>
            return (Target => Struct_Actual.Container.Last_Element);
      end case;
   end;
   
   ----------------------------------------------------------------------
   
   function Next (Object: Structure_Iterator; Position: JSON_Structure_Cursor) 
                 return JSON_Structure_Cursor
   is begin
      if Object.Structure_Root /= Position.Target.Parent then
         raise Constraint_Error with
           "Cursor does not belong to iterated structure.";
      end if;
      
      return (Target => Position.Target.Next);
   end Next;
   
   ----------------------------------------------------------------------
   
   function Previous (Object  : Structure_Iterator; 
                      Position: JSON_Structure_Cursor) 
                     return JSON_Structure_Cursor
   is begin
      if Object.Structure_Root /= Position.Target.Parent then
         raise Constraint_Error with
           "Cursor does not belong to iterated structure.";
      end if;
      
      return (Target => Position.Target.Prev);
   end;
   
   ------------------------
   -- Structure Building --
   ------------------------
   
   function Append_Null_Member (Structure: in out JSON_Mutable_Structure;
                                Name     : in     JSON_String_Value)
                               return JSON_Value_Reference
   is
      Struct_Actual: JSON_Value renames Structure.Structure_Root.all;
      New_Node: Node;
   begin
      if Name'Length = 0 then
         raise Constraint_Error with "Names must not be empty";
      elsif Struct_Actual.Container.Kind /= JSON_Object then
         raise Constraint_Error with "Structure is not an object.";
      end if;
      
      New_Node := Append_Node (Structure.Structure_Root);
      Slab_Strings.Append (New_Node.Name, Name);
      
      if not Structure.Codec_Constant.Write_Only then
         Node_Hash_Maps.Register
           (Path_Map   => Structure.Codec_Mutable.Path_Map,
            Registrant => New_Node);
      end if;
      
      return JSON_Value_Reference'(Ref => New_Node);
   end Append_Null_Member;
   
   ----------------------------------------------------------------------
   
   function Append_Null_Element (Structure: in out JSON_Mutable_Structure)
                                return JSON_Value_Reference
   is
      Struct_Actual: JSON_Value renames Structure.Structure_Root.all;
      New_Node: Node;
   begin
      if Struct_Actual.Container.Kind /= JSON_Array then
         raise Constraint_Error with "Structure is not an array.";
      end if;
      
      New_Node := Append_Node (Structure.Structure_Root);
      
      if not Structure.Codec_Constant.Write_Only then
         Node_Hash_Maps.Register 
           (Path_Map   => Structure.Codec_Mutable.Path_Map,
            Registrant => New_Node);
      end if;
      
      return JSON_Value_Reference'(Ref => New_Node);
   end Append_Null_Element;
   
   ----------------------------------------------------------------------

   function Append_Structural_Member 
     (Structure: in out JSON_Mutable_Structure;
      Name     : in     JSON_String_Value;
      Kind     : in     JSON_Structure_Kind)
     return JSON_Value_Reference
   is 
      New_Struct: JSON_Value_Reference 
        := Structure.Append_Null_Member (Name);

   begin
      case Kind is
         when JSON_Object =>
            New_Struct.Container := (Kind => JSON_Object, others => <>);
            Node_Hash_Maps.Setup (Map     => New_Struct.Container.Name_Map,
                                  Subpool => New_Struct.Codec_Subpool);
         when JSON_Array =>
            New_Struct.Container := (Kind => JSON_Array, others => <>);
            Node_Hash_Maps.Setup (Map     => New_Struct.Container.Index_Map,
                                  Subpool => New_Struct.Codec_Subpool);
      end case;

      return (Ref => New_Struct.Ref.all'Unchecked_Access);
      -- THIS IS SAFE BECAUSE: Ref points at a Node, which is always allocated
      -- dynamically on a subpool owned by the codec. Since
      -- JSON_Mutable_Structure is limited, and non-default disciminated
      -- objects cannot be assigned except through initialization, it is not
      -- possible for any JSON_Value_Reference returned here to end up as a
      -- dangling reference. However since we are using an anonymous access
      -- type, the compiler doesn't know that.
   end;
   
   ----------------------------------------------------------------------
   
   function Append_Structural_Element 
     (Structure: in out JSON_Mutable_Structure;
      Kind     : in     JSON_Structure_Kind)
     return JSON_Value_Reference
   is
      New_Struct: JSON_Value_Reference 
        := Structure.Append_Null_Element;

   begin
      case Kind is
         when JSON_Object =>
            New_Struct.Container := (Kind => JSON_Object, others => <>);
            Node_Hash_Maps.Setup (Map     => New_Struct.Container.Name_Map,
                                  Subpool => New_Struct.Codec_Subpool);
         when JSON_Array =>
            New_Struct.Container := (Kind => JSON_Array, others => <>);
            Node_Hash_Maps.Setup (Map     => New_Struct.Container.Index_Map,
                                  Subpool => New_Struct.Codec_Subpool);
      end case;
      
      return (Ref => New_Struct.Ref.all'Unchecked_Access);
      -- THIS IS SAFE BECAUSE: Ref points at a Node, which is always allocated
      -- dynamically on a subpool owned by the codec. Since
      -- JSON_Mutable_Structure is limited, and non-default disciminated
      -- objects cannot be assigned except through initialization, it is not
      -- possible for any JSON_Value_Reference returned here to end up as a
      -- dangling reference. However since we are using an anonymous access
      -- type, the compiler doesn't know that.
   end;
   
   --
   -- Unbounded_JSON_Codec
   --
   
   -----------------
   -- Path_Exists --
   -----------------
   
   function Path_Exists (Codec: Unbounded_JSON_Codec;
                         Path : in JSON_String_Value)
                        return Boolean
   is
      use Node_Hash_Maps;
   begin
      if Codec.Write_Only then
         raise Constraint_Error with "Cannot index. Codec is write-only.";
      end if;
      
      return Lookup_By_Path (Path_Map => Codec.Path_Map,
                             Path     => Path)
        /= null;
   end;
   
   ------------
   -- Lookup --
   ------------
   
   function Lookup (Codec: aliased in out Unbounded_JSON_Codec;
                    Path :         in     JSON_String_Value)
                   return JSON_Value_Reference
   is 
      use Node_Hash_Maps;
   begin
      if Codec.Write_Only then
         raise Constraint_Error with "Cannot index. Codec is write-only.";
      end if;
      
      return JSON_Value_Reference'
        (Ref => Lookup_By_Path (Path_Map => Codec.Path_Map,
                                Path     => Path));
   end;
   
   ----------------------------------------------------------------------
   
   function Constant_Lookup (Codec: aliased Unbounded_JSON_Codec;
                             Path :         JSON_String_Value)
                            return JSON_Value_Constant_Reference
   is 
      use Node_Hash_Maps;
   begin
      if Codec.Write_Only then
         raise Constraint_Error with "Cannot index. Codec is write-only.";
      end if;
      
      return JSON_Value_Constant_Reference'
        (Ref => Lookup_By_Path (Path_Map => Codec.Path_Map,
                                Path     => Path));
   end;
   
   ----------
   -- Root --
   ----------
   
   function Root (Codec: aliased in out Unbounded_JSON_Codec)
                 return JSON_Mutable_Structure'Class
   is (JSON_Mutable_Structure'(Structure_Root => Codec.Root_Node,
                               Codec_Constant => Codec'Unchecked_Access,
                               Codec_Mutable  => Codec'Unchecked_Access));
   
   -- THIS IS SAFE BECAUSE: JSON_Mutable_Structure'Class is a limited type,
   -- and so this function must be initializing a new declaration, which
   -- ensures Codec exists for at least as long as the returned object.
   --
   -- If Unchecked_Deallocate is used on Codec, use of 'Access would not be
   -- protective anyways.

   ----------------------------------------------------------------------
   
   function Constant_Root (Codec: aliased Unbounded_JSON_Codec)
                           return JSON_Constant_Structure'Class
   is (JSON_Constant_Structure'(Structure_Root => Codec.Root_Node,
                                Codec_Constant => Codec'Unchecked_Access));
   
   -- THIS IS SAFE BECAUSE: JSON_Constant_Structure'Class is a limited type,
   -- and so this function must be initializing a new declaration, which
   -- ensures Codec exists for at least as long as the returned object.
   --
   -- If Unchecked_Deallocate is used on Codec, use of 'Access would not be
   -- protective anyways.
   
   
   -----------
   -- Delve --
   -----------
   
   function Delve (Codec    : aliased in out Unbounded_JSON_Codec;
                   Structure: aliased in out JSON_Value'Class)
                  return JSON_Mutable_Structure'Class
   is begin
      return JSON_Mutable_Structure'
        (Structure_Root => Seek_Node (Codec => Codec,
                                      Value => Structure),
         Codec_Constant => Codec'Unchecked_Access,
         Codec_Mutable  => Codec'Unchecked_Access);
      
      -- THIS IS SAFE BECAUSE: JSON_Mutable_Structure'Class is a limited type,
      -- and so this function must be initializing a new declaration, which
      -- ensures Codec exists for at least as long as the returned object.
      --
      -- If Unchecked_Deallocate is used on Codec, use of 'Access would not be
      -- protective anyways.
   end Delve;
   
   ----------------------------------------------------------------------
   
   function Constant_Delve (Codec    : aliased Unbounded_JSON_Codec;
                            Structure: aliased JSON_Value'Class)
                           return JSON_Constant_Structure'Class
   is begin
      return JSON_Constant_Structure'
        (Structure_Root => Seek_Node (Codec => Codec,
                                      Value => Structure),
         Codec_Constant => Codec'Unchecked_Access);
      
      -- THIS IS SAFE BECAUSE: JSON_Constant_Structure'Class is a limited type,
      -- and so this function must be initializing a new declaration, which
      -- ensures Codec exists for at least as long as the returned object.
      --
      -- If Unchecked_Deallocate is used on Codec, use of 'Access would not be
      -- protective anyways.
   end Constant_Delve;
   
   -----------
   -- Valid --
   -----------
   
   function Valid (Codec: Unbounded_JSON_Codec) return Boolean is
      use Parsers;
   begin
      return Last_Indication (Codec.Parser) /= Invalid;
   end;
   
   -------------------------------
   -- Invalid_Because_End_Error --
   -------------------------------
   
   function Invalid_Because_End_Error (Codec: Unbounded_JSON_Codec)
                                      return Boolean
   is (Codec.End_Error);
   
   -------------------
   -- Error_Message --
   -------------------
   
   procedure Set_Parser_Error (Codec: in out Unbounded_JSON_Codec) is
      use Ada.Strings, Ada.Strings.Fixed;
      
      Pos: constant Parsers.Text_Position
        := Parsers.Last_Position (Codec.Parser);
      
      Parser_Error_String: constant String
        := "Invalid JSON: "
          &
          (if Pos.Overflow then "(Position Overflow):"
           else Trim (Positive'Image (Pos.Line), Left) & ':' 
              & Trim (Natural'Image (Pos.Column), Left) & ':')
          & ' ' 
          & Parsers.Invalid_Reason (Codec.Parser);
   begin
      Slab_Strings.Clear (Codec.Error);
      Slab_Strings.Append
        (Target => Codec.Error,
         Source => To_Wide_Wide_String (Parser_Error_String));
   end Set_Parser_Error;
   
   ----------------------------------------------------------------------
   
   function Error_Message (Codec: Unbounded_JSON_Codec) return String is
      use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
   begin
      if Codec.Valid then
         raise Constraint_Error with "Error_Message can only be invoked if "
           & "the Codec is inValid.";
      end if;
      
      return Encode (Slab_Strings.To_JSON_String (Codec.Error));
   end Error_Message;
   
   
   ---------------
   -- Serialize --
   ---------------
   
   -- Single Node Encoding ----------------------------------------------
   
   function Encode_Node (Target: not null Node) return Wide_Wide_String is
      -- Outputs the encoded value of Target, but does not include any
      -- trailing or leading structural characters (such as separators).
      -- Only a name (and name separator) is added when appropriate.
      --
      -- However, if Target is an Object or Array, '{' or '[' is output,
      -- respectively
      
      use JSON.Standards;
      
      function Trim (Source: in Wide_Wide_String;
                     Side  : in Ada.Strings.Trim_End := Ada.Strings.Left)
                    return Wide_Wide_String
        renames Ada.Strings.Wide_Wide_Fixed.Trim;
      
      Is_Named: constant Boolean 
        := Target.Parent /= null 
          and then Target.Parent.Container.Kind = JSON_Object;
      

      
      
      function Format_String (SS: Slab_Strings.Slab_String)
                             return JSON_String_Value
      is
         use JSON.Standards;
         use Ada.Strings.Wide_Wide_Fixed;
         
         package WWM renames Ada.Strings.Wide_Wide_Maps;
         
         -- This role of this function is to prepare a slab string for actual
         -- JSON output. This means adding the quotation marks on each end,
         -- as well as scanning for anything that needs to be escaped, and
         -- converting those into the correct escape sequence.
         
         Original_Length: constant Natural := Slab_Strings.Length (SS);
         Original: JSON_String_Value (1 .. Original_Length);
         Original_Last: Natural := Original'First - 1;
         Original_Mark: Natural;
         
         Copy_Length: Natural;
         
         Need_Escaping: Natural;
         Formatted_Length: Natural;
         Formatted_Last: Natural;
      begin
         Slab_Strings.To_JSON_String (Source => SS, Target => Original);
         
         Need_Escaping := Count (Source => Original,
                                 Set    => Escape_Serialize_Set);
         
         if Need_Escaping = 0 then
            -- Shortcut!
            return Quotation_Mark & Original & Quotation_Mark;
         end if;
         
         -- We have escaping, so we need to do more work
         
         Formatted_Length := Original_Length + Need_Escaping + 2;
         -- Each escaped character needs an extra \ added, and the 2 is for
         -- the two quotation marks that enclose the entire string
         
         return Formatted: JSON_String_Value (1 .. Formatted_Length) do
            
            Formatted(Formatted'First) := Quotation_Mark;
            Formatted(Formatted'Last)  := Quotation_Mark;
            Formatted_Last := Formatted'First;
            
            while Original_Last < Original'Last loop
               Original_Mark := Index (Source => Original,
                                       Set    => Escape_Serialize_Set,
                                       From   => Original_Last + 1);
               
               exit when Original_Mark = 0;
               
               -- Copy everything up-to index to Formatted (if there is
               -- anything to copy)
               Copy_Length := Original_Mark - 1 - Original_Last;
               
               if Copy_Length > 0 then
                  Formatted 
                    (Formatted_Last + 1 .. Formatted_Last + Copy_Length)
                    := Original
                      (Original_Last + 1 .. Original_Last + Copy_Length);
                  
                  Formatted_Last := Formatted_Last + Copy_Length;
                  Original_Last  := Original_Last + Copy_Length;
               end if;
               
               -- Now we add in the '\'
               Formatted(Formatted_Last + 1) := Escape_Symbol;
               Formatted_Last := Formatted_Last + 1;
               
               -- Now we convert the original character to the proper encoded
               -- form with a map
               
               pragma Assert (Original_Mark = Original_Last + 1);
               Formatted (Formatted_Last + 1) := WWM.Value 
                 (Map     => Escape_Code_Reverse_Map, 
                  Element => Original (Original_Mark));
               
               Formatted_Last := Formatted_Last + 1;
               Original_Last  := Original_Mark;
               
            end loop;
         
            -- Finish-up with a final copy if needed
            if Original_Last < Original'Last then
               -- If our math is right, this should always work-out correctly
               Formatted (Formatted_Last + 1 .. Formatted'Last - 1)
                 := Original (Original_Last + 1 .. Original'Last);
            end if;
         end return;
         
      end Format_String;
      
      
      function Leader return Wide_Wide_String is
        (if Is_Named then 
            Format_String (Target.Name)
            & Name_Separator
         else "");
      
   begin
      
      case Target.Container.Kind is
         when JSON_Object => 
            return Leader & Begin_Object;
         when JSON_Array =>
            return Leader & Begin_Array;
         when JSON_String =>
            return Leader
              & Format_String (Target.Container.String_Value);
         when JSON_Integer =>
            return Leader & Trim 
              (JSON_Integer_Value'Wide_Wide_Image 
                 (Target.Container.Integer_Value));
         when JSON_Float =>
            return Leader & Trim 
              (JSON_Float_Value'Wide_Wide_Image
                 (Target.Container.Float_Value));
         when JSON_Boolean =>
            return Leader 
              & (if Target.Container.Boolean_Value then Literal_True
                 else                                   Literal_False);
         when JSON_Null =>
            return Leader & Literal_Null;
      end case;
   end Encode_Node;
   
   -- Generic Serializer ------------------------------------------------
   
   generic
      with procedure Output_Segment (Output: UTF_8_String);
   procedure Generic_Serializer (Codec: Unbounded_JSON_Codec);
   
   procedure Generic_Serializer (Codec: Unbounded_JSON_Codec) is
      use JSON.Standards;
      
      function UTF_8 (Item      : Wide_Wide_String;
                      Output_BOM: Boolean := False)
                     return UTF_8_String
        renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode;
      
      procedure Recursive_Descend (Down_From: not null Node);
      -- Down_From shall be a JSON_Object or JSON_Array. 
      
      procedure Recursive_Descend (Down_From: not null Node) is
         Finger: Node;
      begin
         case JSON_Structure_Kind (Down_From.Container.Kind) is
            when JSON_Object => Finger := Down_From.Container.First_Member;
            when JSON_Array  => Finger := Down_From.Container.First_Element;
         end case;
         
         while Finger /= null loop
            Output_Segment (UTF_8 (Encode_Node (Finger)));
            
            if Finger.Kind in JSON_Structure_Kind then
               Recursive_Descend (Finger);
            end if;
            
            Finger := Finger.Next;
            
            if Finger /= null then
               Output_Segment 
                 (UTF_8 (Wide_Wide_String'(1 => Value_Separator)));
            end if;
         end loop;
         
         case JSON_Structure_Kind (Down_From.Container.Kind) is
            when JSON_Object =>
               Output_Segment (UTF_8 (Wide_Wide_String'(1 => End_Object)));
            when JSON_Array =>
               Output_Segment (UTF_8 (Wide_Wide_String'(1 => End_Array)));
         end case;
      end Recursive_Descend;
      

   begin
      -- We "hand craft" the root node opening. The closing will be handled by
      -- Recursive_Descend.
      
      case JSON_Structure_Kind (Codec.Root_Node.Container.Kind) is
         when JSON_Object =>
            Output_Segment (UTF_8 (Wide_Wide_String'(1 => Begin_Object)));
         when JSON_Array  =>
            Output_Segment (UTF_8 (Wide_Wide_String'(1 => Begin_Array)));
      end case;
      
      Recursive_Descend (Down_From => Codec.Root_Node);
   end Generic_Serializer;
   
   -- Serialized_Length -------------------------------------------------
   
   function  Serialized_Length (Codec: in Unbounded_JSON_Codec) 
                               return Natural
   is 
      Total_Length: Natural := 0;
      
      procedure Meter_Output (Output: UTF_8_String) is
      begin
         Total_Length := Total_Length + Output'Length;
      end;
      
      procedure Dry_Run is new Generic_Serializer (Meter_Output);
   begin
      Dry_Run (Codec);
      pragma Assert (Total_Length >= 2);
      return Total_Length;
   end Serialized_Length;
      
   ----------------------------------------------------------------------
   
   procedure Serialize (Codec : in    Unbounded_JSON_Codec;
                        Output:   out UTF_8_String)
   is 
      Last: Natural := Output'First - 1;
      
      procedure Append_Segment (Segment: UTF_8_String) is
         First: constant Natural := Last + 1;
      begin
         Last := First + Segment'Length - 1;
         Output (First .. Last) := Segment;
      end;
      
      procedure Serialize_Actual is new Generic_Serializer (Append_Segment);
   begin
      Serialize_Actual (Codec);
      if Last < Output'Last then
         Output (Last + 1 .. Output'Last) := (others => ' ');
      end if;
   end;
   
   ----------------------------------------------------------------------
   
   function  Serialize (Codec: in Unbounded_JSON_Codec) 
                       return UTF_8_String 
   is begin
      return Output_Buffer: UTF_8_String (1 .. Codec.Serialized_Length) do
         Codec.Serialize (Output_Buffer);
      end return;
   end;
   
   ----------------------------------------------------------------------
   
   procedure Serialize 
     (Output_Stream: not null access Ada.Streams.Root_Stream_Type'Class;
      Codec        : in Unbounded_JSON_Codec)
   is
      procedure Stream_Segment (Output: UTF_8_String) is
      begin
         UTF_8_String'Write (Output_Stream, Output);
      end;
      
      procedure Serialize_Actual is new Generic_Serializer (Stream_Segment);
   begin
      Serialize_Actual (Codec);
   end Serialize;
   
   -----------------
   -- Parser_Push --
   -----------------
   
   -- This is the core subprogram for driving the parser finite state machine,
   -- used by all deserialization operations.
   

   
   procedure Parser_Push (Codec         : in out Unbounded_JSON_Codec'Class;
                          Next_Character: in     Wide_Wide_Character;
                          Indication    : in out Parsers.Operator_Indication)
   with 
     Inline, 
     Pre  => Parsers."=" (Indication, Parsers.Ready),
     Post => Indication in Parsers.Ready | Parsers.Done | Parsers.Invalid
   is
      -- Parser_Push is the meat of a single character loop, where simply the
      -- loop should terminate when Indication is Invalid or Done.
      --
      -- The purpose of Exec_Push is to improve performance by reducing
      -- stack frame movements and procedure calls compared to just calling
      -- "FSM_Push" for each character.
      --
      -- Indication should be initialized to the Parser_FSM's Last_Indication
      -- before the first call of Exec_Push
      
      use Parsers;
      Parser: Parser_FSM renames Codec.Parser;
      Current_Structure: Node renames Codec.Current_Structure;
      Output_Recipient: Node := null;
      
      
      procedure Initialization_Pass is
         -- Invoked on entry until Current_Structure is not null. This
         -- subprogram waits for a Push indication and then allocates and
         -- configures the codec's root
      begin
         Feed (Machine    => Parser,
               Input      => Next_Character,
               Indication => Indication);
         
         case Indication is
            when Ready | Invalid =>
               return;
               
            when Push =>
               Codec.Root_Node := new (Codec.Codec_Subpool) JSON_Value;
               Codec.Root_Node.Index := 0;
               Codec.Root_Node.Codec_Subpool := Codec.Codec_Subpool;
               Codec.Root_Node.Root := Codec.Root_Node; 
               -- Used for IDing the owning Codec only               
               
               case Current_Structure_Kind (Parser) is
                  when JSON_Object =>
                     Codec.Root_Node.Container := (Kind   => JSON_Object,
                                                   others => <>);
                     Node_Hash_Maps.Setup 
                       (Map     => Codec.Root_Node.Container.Name_Map,
                        Subpool => Codec.Codec_Subpool);
                     
                  when JSON_Array =>
                     Codec.Root_Node.Container := (Kind   => JSON_Array,
                                                   others => <>);
                     Node_Hash_Maps.Setup 
                       (Map     => Codec.Root_Node.Container.Index_Map,
                        Subpool => Codec.Codec_Subpool);
               end case;
               
               Acknowledge_Push (Machine    => Parser,
                                 Indication => Indication);
               
               Current_Structure := Codec.Root_Node;
               
            when others =>
               raise Program_Error with "Unexpected indication from the "
                       & "paser state machine.";
         end case;
         
      end Initialization_Pass;
      
      
      procedure Do_Pop is
         -- We often need to do pops after we acknowledge an output, so
         -- this avoids needlessly wrapping the entire post-Feed case statement
         -- in a giant loop.
      begin
         Current_Structure := Current_Structure.Parent;
         -- Note this will never be null if the Parser operates correctly.
         
         Acknowledge_Pop (Machine    => Parser,
                          Popped_To  => Current_Structure.Container.Kind,
                          Indication => Indication);
      end;
   begin
      if Current_Structure = null then
         Initialization_Pass;
         return;
      end if;
      
      Feed (Machine    => Parser,
            Input      => Next_Character,
            Indication => Indication);
      
      case Indication is
         when Ready | Done | Invalid =>
            return;
            
         when Output_Member | Output_Element =>
            Output_Recipient := Append_Node (Current_Structure);
            
            if Indication = Output_Member then
               Output_Name (Machine => Parser,
                            Target  => Output_Recipient.Name);
            end if;
            
            case Output_Kind (Parser) is
               when JSON_Object | JSON_Array =>
                  raise Program_Error with 
                    "Parser FSM violated Output_Kind postcondition";
                  
               when JSON_String =>
                  Output_Recipient.Container := (Kind   => JSON_String, 
                                                 others => <>);
                  Output_Value 
                    (Machine => Parser,
                     Target  => Output_Recipient.Container.String_Value);
               when JSON_Integer =>
                  Output_Recipient.Container := (Kind   => JSON_Integer,
                                                 others => <>);
                  Output_Recipient.Container.Integer_Value
                    := Output_Value (Parser);
               when JSON_Float =>
                  Output_Recipient.Container := (Kind   => JSON_Float,
                                                 others => <>);
                  Output_Recipient.Container.Float_Value
                    := Output_Value (Parser);
               when JSON_Boolean =>
                  Output_Recipient.Container := (Kind   => JSON_Boolean,
                                                 others => <>);
                  Output_Recipient.Container.Boolean_Value
                    := Output_Value (Parser);
               when JSON_Null =>
                  null;
            end case;
            
            Acknowledge_Output (Machine    => Parser,
                                Indication => Indication);
            
            if not Codec.Write_Only then
               Node_Hash_Maps.Register (Path_Map   => Codec.Path_Map,
                                        Registrant => Output_Recipient);
            end if;
            
            if Indication = Pop then
               Do_Pop;
            end if;
            
         when Push =>
            Current_Structure := Append_Node (Current_Structure);
              
            if Current_Structure.Parent.Container.Kind = JSON_Object then
               Output_Name (Machine => Parser,
                            Target  => Current_Structure.Name);
            end if;
            
            case Current_Structure_Kind (Parser) is
               when JSON_Object =>
                  Current_Structure.Container := (Kind   => JSON_Object,
                                                  others => <>);
                  Node_Hash_Maps.Setup 
                    (Map     => Current_Structure.Container.Name_Map,
                     Subpool => Current_Structure.Codec_Subpool);
               when JSON_Array =>
                  Current_Structure.Container := (Kind   => JSON_Array,
                                                  others => <>);
                  Node_Hash_Maps.Setup 
                    (Map     => Current_Structure.Container.Index_Map,
                     Subpool => Current_Structure.Codec_Subpool);
            end case;
            
            if not Codec.Write_Only then
               Node_Hash_Maps.Register (Path_Map   => Codec.Path_Map,
                                        Registrant => Current_Structure);
            end if;

            Acknowledge_Push (Machine    => Parser,
                              Indication => Indication);
            
         when Pop =>
            Do_Pop;

      end case;
   end Parser_Push;
   
   -----------------
   -- Deserialize --
   -----------------
   
   function Deserialize (Input: UTF_8_String) return Unbounded_JSON_Codec is
      use Parsers;
      
      WW_Characters: constant Wide_Wide_String
        := Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode (Input);
      
      Indication: Operator_Indication; 
   begin
      return Codec: Unbounded_JSON_Codec do
         Indication := Last_Indication (Codec.Parser);
         
         for Next_Character of WW_Characters loop
            Parser_Push (Codec, Next_Character, Indication);
            exit when Indication /= Ready;
         end loop;
         
         pragma Assert (Indication in Done | Invalid | Ready);
         
         if Indication = Invalid then
            Set_Parser_Error (Codec);
         elsif Indication = Ready then
            Codec.End_Error := True;
            Slab_Strings.Clear (Codec.Error);
            Slab_Strings.Append (Codec.Error,
                                 "Serialized JSON text ended unexpectedly");
            Parsers.Emergency_Stop (Codec.Parser);
         end if;
         
      exception
         when e: others =>
            Slab_Strings.Clear (Codec.Error);
            Slab_Strings.Append
              (Codec.Error, 
               "Deserialization failed with an exception: "
                 & To_Wide_Wide_String 
                   (Ada.Exceptions.Exception_Information (e)));
            Parsers.Emergency_Stop (Codec.Parser);
      end return;
   end;
   
   ----------------------------------------------------------------------
   
   
   generic
      Slow_Loris_Protection: in Boolean  := False;
   
   procedure Generic_Stream_Deserialize
     (Codec : in out Unbounded_JSON_Codec;
      Source: not null access Ada.Streams.Root_Stream_Type'Class;
      Decode_Time_Budget: in Duration := 0.0);
   
   procedure Generic_Stream_Deserialize
     (Codec : in out Unbounded_JSON_Codec;
      Source: not null access Ada.Streams.Root_Stream_Type'Class;
      Decode_Time_Budget: in Duration := 0.0)
   is
      use Parsers;
      use type Ada.Real_Time.Time;
      
      package UTF8 renames Unicode.UTF8_Stream_Decoder;
      
      Deadline: Ada.Real_Time.Time;
      Indication: Operator_Indication := Last_Indication (Codec.Parser);
   begin 
      if Slow_Loris_Protection then
         Deadline := Ada.Real_Time.Clock 
           + Ada.Real_Time.To_Time_Span (Decode_Time_Budget);
      end if;
      
      while Indication = Ready loop
         Parser_Push (Codec          => Codec, 
                      Next_Character => UTF8.Decode_Next (Source),
                      Indication     => Indication);
         
         if Slow_Loris_Protection then
            if Ada.Real_Time.Clock > Deadline then
               Codec.End_Error := False;
               Slab_Strings.Clear (Codec.Error);
               Slab_Strings.Append
                 (Codec.Error,
                  "Deserialization did not complete before the deadline.");
               Parsers.Emergency_Stop (Codec.Parser);
               return;
            end if;
         end if;
      end loop;
      
      pragma Assert (Indication in Done | Invalid);
      
      if Indication = Invalid then
         Set_Parser_Error (Codec);
      end if;
         
   exception
      when Ada.IO_Exceptions.End_Error =>
         Codec.End_Error := True;
         Slab_Strings.Clear (Codec.Error);
         Slab_Strings.Append (Codec.Error,
                              "Serialized JSON stream ended unexpectedly");
         Parsers.Emergency_Stop (Codec.Parser);
      when e: others =>
         Slab_Strings.Clear (Codec.Error);
         Slab_Strings.Append
           (Codec.Error, 
            "Deserialization failed with an exception: "
              & To_Wide_Wide_String 
                (Ada.Exceptions.Exception_Information (e)));
         Parsers.Emergency_Stop (Codec.Parser);
   end Generic_Stream_Deserialize;
   
   ----------------------------------------------------------------------
   
   procedure Normal_Stream_Deserialize is new Generic_Stream_Deserialize;
   procedure Protected_Stream_Deserialize is new Generic_Stream_Deserialize
     (Slow_Loris_Protection => True);
   
   ----------------------------------------------------------------------
   
   
   function Deserialize
     (Source: not null access Ada.Streams.Root_Stream_Type'Class)
     return Unbounded_JSON_Codec
   is begin
      return Codec: Unbounded_JSON_Codec do
         Normal_Stream_Deserialize (Codec, Source);
      end return;
   end;
   
   ----------------------------------------------------------------------
   
   function Deserialize
     (Source: not null access Ada.Streams.Root_Stream_Type'Class;
      Limits: Codec_Limits)
     return Unbounded_JSON_Codec
   is begin
      return Codec: Unbounded_JSON_Codec do
         Parsers.Configure_Limits (Machine => Codec.Parser,
                                   Limits  => Limits);
         
         Normal_Stream_Deserialize (Codec, Source);
      end return;
   end;
   
   ----------------------------------------------------------------------
   
   function Time_Bounded_Deserialize
     (Source: not null access Ada.Streams.Root_Stream_Type'Class;
      Budget: in Duration)
     return Unbounded_JSON_Codec
   is begin
      return Codec: Unbounded_JSON_Codec do
         Protected_Stream_Deserialize 
           (Codec, Source, Decode_Time_Budget => Budget);
      end return;
   end;
   
   ----------------------------------------------------------------------
   
   function Time_Bounded_Deserialize
     (Source: not null access Ada.Streams.Root_Stream_Type'Class;
      Limits: Codec_Limits;
      Budget: in Duration)
     return Unbounded_JSON_Codec
   is begin
      return Codec: Unbounded_JSON_Codec do
         Parsers.Configure_Limits (Machine => Codec.Parser,
                                   Limits  => Limits);
         
         Protected_Stream_Deserialize
           (Codec, Source, Decode_Time_Budget => Budget);
      end return;
   end;
   
   
   ----------------------------------------------------------------------
   
   
   ---------------------
   -- Disallowed_Read --
   ---------------------
   
   procedure Disallowed_Read
     (Stream: not null access Ada.Streams.Root_Stream_Type'Class;
      Item  : out Unbounded_JSON_Codec)
   is begin
      raise Program_Error with "'Read is not allowed for Unbounded_JSON_Codec.";
   end;
   
   ------------------
   -- Initializers --
   ------------------
   
   procedure Initialize (Codec: in out Unbounded_JSON_Codec) is
   begin
      Codec.Codec_Subpool := Slab_Pool.Create_Subpool;
      Slab_Strings.Setup (Target => Codec.Path_Buffer,
                          Subpool => Codec.Codec_Subpool);
      Slab_Strings.Setup (Target => Codec.Error,
                          Subpool => Codec.Codec_Subpool);
      Node_Hash_Maps.Setup (Map     => Codec.Path_Map,
                            Subpool => Codec.Codec_Subpool);
      Parsers.Setup_Buffers (Machine => Codec.Parser,
                             Config  => Codec.Codec_Subpool);
      
   end Initialize;
   
   ----------------------------------------------------------------------
   
   function Construction_Codec (Format    : JSON_Structure_Kind := JSON_Object;
                                Write_Only: Boolean             := True)
                               return Unbounded_JSON_Codec
   is begin
      return Codec: Unbounded_JSON_Codec do
         Codec.Write_Only := Write_Only;
         
         -- Hand-craft the root node
         Codec.Root_Node := new (Codec.Codec_Subpool) JSON_Value;
         
         Codec.Root_Node.Root          := Codec.Root_Node;
         Codec.Root_Node.Index         := 0;
         Codec.Root_Node.Codec_Subpool := Codec.Codec_Subpool;
         
         case Format is
            when JSON_Object =>
               Codec.Root_Node.Container := (Kind => JSON_Object, others => <>);
               Node_Hash_Maps.Setup 
                 (Map     => Codec.Root_Node.Container.Name_Map,
                  Subpool => Codec.Codec_Subpool);
            when JSON_Array =>
               Codec.Root_Node.Container := (Kind => JSON_Array, others => <>);
               Node_Hash_Maps.Setup 
                 (Map     => Codec.Root_Node.Container.Index_Map,
                  Subpool => Codec.Codec_Subpool);
         end case;
         
         Codec.Current_Structure := Codec.Root_Node;
         -- Note that this is technically not "necessary" as it is impossible
         -- for a resular Unbounded_JSON_Codec to invoke the parser after being
         -- initialized (as is done from here), but if it was possible, and
         -- this was not set, bad things would happen. So why not?
      end return;
   end Construction_Codec;
   
   --------------
   -- Finalize --
   --------------
   
   procedure Finalize (Codec: in out Unbounded_JSON_Codec) is
   begin
      Ada.Unchecked_Deallocate_Subpool (Codec.Codec_Subpool);
      -- Everthing in one go!
   end Finalize;
   
   --
   -- Unbounded_FSM_JSON_Codec
   --
   
   -----------------------
   -- Input_Limited_FSM --
   -----------------------
   
   function Input_Limited_FSM (Limits: Codec_Limits)
                              return Unbounded_FSM_JSON_Codec 
   is begin
      return Codec: Unbounded_FSM_JSON_Codec do
         Parsers.Configure_Limits (Machine => Codec.Parser,
                                   Limits  => Limits);
      end return;
   end;
   
   ----------------------
   -- Root_Initialized --
   ----------------------
   
   function Root_Initialized (Codec: Unbounded_FSM_JSON_Codec) return Boolean
   is (Codec.Root_Node /= null);
   
   --------------
   -- FSM_Push --
   --------------
   
   procedure FSM_Push (Codec         : in out Unbounded_FSM_JSON_Codec;
                       Next_Character: in     Wide_Wide_Character;
                       Halt          :    out Boolean)
   is
      use Parsers;
      Indication: Operator_Indication := Last_Indication (Codec.Parser);
   begin
      Parser_Push (Codec, Next_Character, Indication);
      Halt := Indication /= Ready;
      pragma Assert (if Halt then Indication in Done | Invalid);
   end;
   
   ----------------------------------------------------------------------
   
   procedure FSM_Push (Codec          : in out Unbounded_FSM_JSON_Codec;
                       Next_Characters: in     Wide_Wide_String;
                       Halt           :    out Boolean)
   is
      use Parsers;
      Indication: Operator_Indication := Last_Indication (Codec.Parser);
   begin
      for Next_Character of Next_Characters loop
         exit when Indication /= Ready;
         Parser_Push (Codec, Next_Character, Indication);
      end loop;
      
      Halt := Indication /= Ready;
      pragma Assert (if Halt then Indication in Done | Invalid);
   end;
   
   
end JSON.Unbounded_Codecs;
