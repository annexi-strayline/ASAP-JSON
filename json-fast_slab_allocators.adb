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

with Ada.Unchecked_Deallocation;

with AURA.JSON;

package body JSON.Fast_Slab_Allocators is
   
   --
   -- Slab Data Structures
   --
   
   package Configuration renames AURA.JSON.Configuration;
   
   Slab_Size     : constant := Configuration.Unbounded_Codec_Slab_Size;
   Slab_Alignment: constant := Configuration.Unbounded_Codec_Slab_Alignment;
   
   subtype Slab_Offset is Storage_Offset range 1 .. Slab_Size;
   
   type Slab_Data is array (Slab_Offset) of aliased Storage_Element with
     Alignment => Slab_Alignment;
   
   pragma Assert (Slab_Data'Size = Storage_Element'Size * Slab_Size);
                 
   -- Note that the above definition is a definite array, thus it does not need
   -- to store any bounds, and Storage_Offset should be naturally aligned (and
   -- thus naturally packed);
   
   type Slab_Data_Access is access Slab_Data;
   
   -- Slab_Size and Slab_Alignment should be such that we allocate a large
   -- enough slab for the Global sized hash tables, and that we are aligned
   -- on the systems pages.
   --
   -- As a matter of interest: this makes a 1 MiB slab with 4 KiB alignment
   -- ideal for nearly all architectures that have any importance both in the
   -- present, and probably for the medium-distant future.
   
   type Slab_Descriptor;
   type Slab_Handle is access Slab_Descriptor;
   
   type Slab_Descriptor is
      record
         Slab     : Slab_Data_Access := new Slab_Data;
         Next_Free: Slab_Offset      := Slab_Offset'First;
         Prev_Slab: Slab_Handle      := null;
         -- The previously allocated full/nearly full slab in the stack. This
         -- slab, and it's Prev_Slab are scanned for space on each allocation.
         -- The entire stack is saved here to allow deallocation of the subpool
      end record;
   
   type Slab_Subpool is new Root_Subpool with
      record
         Slab_Stack: Slab_Handle := new Slab_Descriptor;
      end record;
   
   type Slab_Subpool_Handle is access all Slab_Subpool;
   
   ----------
   -- Push --
   ----------
   
   -- Push allocates the next slab after the existing top of the Slab_Stack
   -- is exhausted
   
   procedure Push (Subpool: in out Slab_Subpool) is
      New_Slab: Slab_Handle := new Slab_Descriptor;
   begin
      New_Slab.Prev_Slab := Subpool.Slab_Stack;
      Subpool.Slab_Stack := New_Slab;
   end;
   
   
   --
   -- Subpool Implementation
   --
   
   --------------------
   -- Create_Subpool --
   --------------------
   
   function Create_Subpool (Pool: in out Slab_Pool_Designator) 
                           return not null Subpool_Handle
   is 
      New_Slab_Subpool: Slab_Subpool_Handle := new Slab_Subpool;
      New_Handle: Subpool_Handle := Subpool_Handle (New_Slab_Subpool);
   begin
      Set_Pool_Of_Subpool (Subpool => New_Handle,
                           To      => Pool);
      return New_Handle;
   end Create_Subpool;
   
   ---------------------------
   -- Allocate_From_Subpool --
   ---------------------------
   
   procedure Allocate_From_Subpool
     (Pool                    : in out Slab_Pool_Designator;
      Storage_Address         :    out System.Address;
      Size_In_Storage_Elements: in     Storage_Count;
      Alignment               : in     Storage_Count;
      Subpool                 : in     not null Subpool_Handle)
   is 
      Subpool_Actual: Slab_Subpool renames Slab_Subpool(Subpool.all);
      
      Block_Start, Block_End: Storage_Offset;
      Selected_Slab         : Slab_Handle;
      Success: Boolean := False;
      
      function Try_Compute_Block (Handle: not null Slab_Handle)
                                 return Boolean 
      with Inline is
         Slab     : Slab_Data   renames Handle.Slab.all;
         Next_Free: Slab_Offset renames Handle.Next_Free;
         
         Alignment_Shift: constant Storage_Offset
           := (Alignment - (Slab(Next_Free)'Address mod Alignment)) 
             mod Alignment;
      begin
         Selected_Slab := Handle;
         
         Block_Start := Next_Free + (Alignment_Shift mod Alignment);
         Block_End   := Block_Start + Size_In_Storage_Elements - 1;
         
         return Block_Start in Slab_Offset and then Block_End in Slab_Offset;
      end;
      
      
   begin
      
      -- We first try to check for any space in the lower two slabs
      
      if Subpool_Actual.Slab_Stack.Prev_Slab /= null then
         if Subpool_Actual.Slab_Stack.Prev_Slab.Prev_Slab /= null then
            -- Try two slabs ago
            Success := Try_Compute_Block 
              (Subpool_Actual.Slab_Stack.Prev_Slab.Prev_Slab);
         end if;
         
         if not Success then
            -- Try one slab ago
            Success := Try_Compute_Block (Subpool_Actual.Slab_Stack.Prev_Slab);
         end if;
         
      end if;
      
      -- Now try the top-level block as the last ditch before allocating a new
      -- slab
      
      if not Success then
         -- try current slab
         Success := Try_Compute_Block (Subpool_Actual.Slab_Stack);
      end if;
      
      -- Last attempt, we allocate a new slab and then try to get a fit. If
      -- that fails, the slab size is simply not large enough for the
      -- allocation.

      if not Success then
         -- We only try this twice (one push). If a fresh slab doesn't work,
         -- the slab size is too small for the allocation!
         Push (Subpool_Actual);
         
         Success := Try_Compute_Block (Subpool_Actual.Slab_Stack);
         
         if not Success then
            raise Storage_Error with 
                "JSON.Unbounded_Codec.Fast_Slab_Allocators: "
              & "slab size is too small!";
         end if;
      end if;
      
      -- We now have valid values
      pragma Assert (Success);
      
      if Block_End + 1 not in Slab_Offset then
         -- Slab is totally full
         pragma Assert (Block_End = Slab_Offset'Last);
         Selected_Slab.Next_Free := Slab_Offset'Last;
      else
         Selected_Slab.Next_Free := Block_End + 1;
      end if;
      
      declare
         Item: Storage_Element renames Selected_Slab.Slab.all(Block_Start);
      begin
         pragma Assert (Block_End - Block_Start + 1 = Size_In_Storage_Elements);
         pragma Assert (Item'Address mod Alignment = 0);
         Storage_Address := Item'Address;
      end;
      
   end Allocate_From_Subpool;
   
   ------------------------
   -- Deallocate_Subpool --
   ------------------------
   
   procedure Deallocate_Subpool (Pool   : in out Slab_Pool_Designator;
                                 Subpool: in out Subpool_Handle)
   is
      procedure Free_Slab is new Ada.Unchecked_Deallocation
        (Object => Slab_Data, Name => Slab_Data_Access);
      
      procedure Free_Descriptor is new Ada.Unchecked_Deallocation
        (Object => Slab_Descriptor, Name => Slab_Handle);
      
      procedure Free_Slab_Subpool is new Ada.Unchecked_Deallocation
        (Object => Slab_Subpool, Name => Slab_Subpool_Handle);
      
      Handle_Actual : Slab_Subpool_Handle := Slab_Subpool_Handle (Subpool);

      Hold: Slab_Handle;
      
   begin
      if Handle_Actual = null then return; end if;
      
      -- Pop the stack and free until we hit the end
      while Handle_Actual.Slab_Stack /= null loop
         Hold := Handle_Actual.Slab_Stack;
         Handle_Actual.Slab_Stack := Hold.Prev_Slab;
         
         Free_Slab       (Hold.Slab);
         Free_Descriptor (Hold);
      end loop;
      
      Subpool := null;
      Free_Slab_Subpool (Handle_Actual);
   end Deallocate_Subpool;
   
end JSON.Fast_Slab_Allocators;
