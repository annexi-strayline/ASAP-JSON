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

separate (JSON.Unbounded_Codecs)

package body Slab_Strings is
   
   -----------------------
   -- Slab_String_Chunk --
   -----------------------
   
   -- We define this as a Taft-amendment to keep the clutter in
   -- Unbounded_Codecs to a minimum
   
   type Slab_String_Chunk is
      record
         Text: JSON_String_Value (1 .. Slab_String_Chunk_Index'Last);
         Next: Slab_String_Chunk_Access := null;
      end record;
   
   -----------
   -- Setup --
   -----------
   
   procedure Setup (Target : in out Slab_String;
                    Subpool: in     not null Subpool_Handle)
   is begin
      Target.Subpool := Subpool;
   end;
   
   ---------
   -- "=" --
   ---------
   
   function "=" (Left, Right: Slab_String) return Boolean is
      Left_Chunk: Slab_String_Chunk_Access := Left.Chain_First;
      Right_Chunk: Slab_String_Chunk_Access := Right.Chain_First;
   begin
      
      if Left_Chunk = null then
         if Right_Chunk = null then
            return True;
         else
            return False;
         end if;
      end if;
      
      -- We don't want to check the Length explicitly since it would require us
      -- to loop through both strings, and then the Left again. We'll just
      -- compare their values directly
      
      loop
         if Left_Chunk = Left.Current_Chunk then
            if Right_Chunk /= Right.Current_Chunk then
               return False;
            end if;
            
            declare
               Left_Slice: JSON_String_Value renames
                 Left_Chunk.Text(1 .. Left.Current_Last);
               Right_Slice: JSON_String_Value renames
                 Right_Chunk.Text(1 .. Right.Current_Last);
            begin
               if Left_Slice = Right_Slice then
                  return True;
               else
                  return False;
               end if;
            end;
            
         else
            if Left_Chunk.Text /= Right_Chunk.Text then
               return False;
            end if;
         end if;
         
         Left_Chunk  := Left_Chunk.Next;
         Right_Chunk := Right_Chunk.Next;
         
      end loop;
   end "=";
   
   ------------
   -- Append --
   ------------
   
   procedure Append (Target: in out Slab_String;
                     Source: in     JSON_String_Value)
   is 
      Start_Source, End_Source: Natural := 0;
      Start_Target, End_Target: Natural := 0;
      
      procedure Next_Chunk is
      begin
         if Target.Current_Chunk.Next = null then
            pragma Assert (Target.Chain_Last = Target.Current_Chunk);
            pragma Assert (Target.Subpool /= null);
            Target.Chain_Last.Next := new (Target.Subpool) Slab_String_Chunk;
            Target.Chain_Last      := Target.Chain_Last.Next;
         end if;
         
         Target.Current_Chunk := Target.Current_Chunk.Next;
         Target.Current_Last := 0;
      end;
   begin
      if Source'Length = 0 then return; end if;
      
      -- We lazily allocate the first chain, waiting until we actually have
      -- something to add
      
      if Target.Chain_First = null then
         pragma Assert (Target.Subpool /= null);
         Target.Chain_First   := new (Target.Subpool) Slab_String_Chunk;
         Target.Chain_Last    := Target.Chain_First;
         Target.Current_Chunk := Target.Chain_First;
      end if;
      
      while End_Source < Source'Last loop
         Start_Source := End_Source + 1;
         End_Source   := Source'Last;
         
         if Target.Current_Last = Slab_String_Chunk_Index'Last then
            Next_Chunk;
         end if;
         
         Start_Target := Target.Current_Last + 1;
         End_Target := Start_Target + (End_Source - Start_Source);
         
         if End_Target not in Slab_String_Chunk_Index then
            End_Target := Slab_String_Chunk_Index'Last;
            End_Source := Start_Source + (End_Target - Start_Target);
         end if;
         
         Target.Current_Chunk.Text (Start_Target .. End_Target)
           := Source (Start_Source .. End_Source);
         
         Target.Current_Last := End_Target;
      end loop;
      
   end Append;
   
   -----------
   -- Clear --
   -----------
   
   procedure Clear (Target: in out Slab_String) is
   begin
      Target.Current_Chunk := Target.Chain_First;
      Target.Current_Last  := 0;
   end;
   
   --------------
   -- Transfer --
   --------------
   
   procedure Transfer (From: in out Slab_String;
                       To  :    out Slab_String)
   is begin
      To   := From;
      From := (Subpool => From.Subpool,
               others  => <>);
   end;
   
   ------------
   -- Length --
   ------------
   
   function Length (S: Slab_String) return Natural is
      Chunk: Slab_String_Chunk_Access := S.Chain_First;
   begin
      if Chunk = null then return 0; end if;
      
      return Len: Natural := 0 do
         loop
            if Chunk = S.Current_Chunk then
               Len := Len + S.Current_Last;
               exit;
            else
               Len := Len + Slab_String_Chunk_Index'Last;
            end if;
            
            Chunk := Chunk.Next;
            -- Not that we shouldn't be able to pass S.Current_Chunk, and
            -- so Chunk.Next should never be null. The null check here serves
            -- as an assertion of sorts
         end loop;
      end return;
   end;
   
   --------------------
   -- To_JSON_String --
   --------------------
   
   procedure To_JSON_String (Source: in     Slab_String;
                             Target:    out JSON_String_Value)
   is 
      Chunk: Slab_String_Chunk_Access := Source.Chain_First;
      Target_Last: Natural := Target'First - 1;
      New_Last: Positive;
   begin
      if Chunk = null then return; end if;
      
      loop
         if Chunk = Source.Current_Chunk then
            pragma Assert (Target_Last + Source.Current_Last = Target'Last);
            
            Target (Target_Last + 1 .. Target_Last + Source.Current_Last)
              := Chunk.Text (1 .. Source.Current_Last);
            exit;
            
         else
            New_Last := Target_Last + Chunk.Text'Length;
            Target (Target_Last + 1 .. New_Last) := Chunk.Text;
            Target_Last := New_Last;
         end if;
         
         Chunk := Chunk.Next;
         -- Not that we shouldn't be able to pass Source.Current_Chunk, and
         -- so Chunk.Next should never be null. The null check here serves
         -- as an assertion of sorts
      end loop;
   end To_JSON_String;
   
   ----------------------------------------------------------------------
   
   function To_JSON_String (Source: in Slab_String) return JSON_String_Value
   is begin
      if Source.Chain_First = null then return ""; end if;
      
      return Target: JSON_String_Value (1 .. Length (Source)) do
         To_JSON_String (Source, Target);
      end return;
   end To_JSON_String;
   
   ------------------
   -- Write_Stream --
   ------------------
   
   procedure Write_Stream 
     (Stream: not null access Ada.Streams.Root_Stream_Type'Class;
      Item  : in Slab_String)
   is 
      Chunk: Slab_String_Chunk_Access := Item.Chain_First;
   begin
      if Chunk = null then return; end if;
      
      loop
         if Chunk = Item.Current_Chunk then
            JSON_String_Value'Write 
              (Stream, Chunk.Text (1 .. Item.Current_Last));
            exit;
         else
            JSON_String_Value'Write (Stream, Chunk.Text);
         end if;
         
         Chunk := Chunk.Next;
         -- Not that we shouldn't be able to pass Item.Current_Chunk, and
         -- so Chunk.Next should never be null. The null check here serves
         -- as an assertion of sorts
      end loop;
   end Write_Stream;
   
end Slab_Strings;
