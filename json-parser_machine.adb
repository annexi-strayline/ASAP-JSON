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
--- * Richard Wai (ANNEXI-STRAYLINE)                                        --
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

with JSON.Standards;

package body JSON.Parser_Machine is
   
   Error_Unexpected_Exception: aliased constant String
     := "An unexpected exception occured.";
   
   package Transitions is
      -- Named according to the state on entry
      procedure Start_Value         (Machine: in out Parser_FSM);
      procedure In_Null             (Machine: in out Parser_FSM);
      procedure In_True             (Machine: in out Parser_FSM);
      procedure In_False            (Machine: in out Parser_FSM);
      procedure In_Integer          (Machine: in out Parser_FSM);
      procedure In_Float            (Machine: in out Parser_FSM);
      procedure In_Integer_Exponent (Machine: in out Parser_FSM);
      procedure In_Float_Exponent   (Machine: in out Parser_FSM);
      procedure Start_String        (Machine: in out Parser_FSM);
      procedure In_String           (Machine: in out Parser_FSM);
      procedure In_String_Escape    (Machine: in out Parser_FSM);
      procedure In_Unicode_Escape   (Machine: in out Parser_FSM);
      procedure After_Name          (Machine: in out Parser_FSM);
      procedure End_Value           (Machine: in out Parser_FSM);
      procedure After_Push          (Machine: in out Parser_FSM);
      procedure After_Value         (Machine: in out Parser_FSM);
      procedure Output              (Machine: in out Parser_FSM)  is null;
      procedure Halt                (Machine: in out Parser_FSM)  is null;
      
      type State_Entry is 
        not null access procedure (Machine: in out Parser_FSM);
      
      Transition_Switch: constant array (Machine_State) of State_Entry
        := (Start_Value         => Start_Value'Access,
            In_Null             => In_Null'Access,
            In_True             => In_True'Access,
            In_False            => In_False'Access,
            In_Integer          => In_Integer'Access,
            In_Float            => In_Float'Access,
            In_Integer_Exponent => In_Integer_Exponent'Access,
            In_Float_Exponent   => In_Float_Exponent'Access,
            Start_String        => Start_String'Access,
            In_String           => In_String'Access,
            In_String_Escape    => In_String_Escape'Access,
            In_Unicode_Escape   => In_Unicode_Escape'Access,
            After_Name          => After_Name'Access,
            End_Value           => End_Value'Access,
            After_Push          => After_Push'Access,
            After_Value         => After_Value'Access,
            Output              => Output'Access,
            Halt                => Halt'Access);
      
   end Transitions;
   
   package body Transitions is separate;
   
   
   --------------
   -- Dispatch --
   --------------
   
   procedure Dispatch (Machine: in out Parser_FSM) with Inline is
   begin
      Transitions.Transition_Switch(Machine.State) (Machine);
   end;
   
   -------------------
   -- Setup_Buffers --
   -------------------
   
   procedure Setup_Buffers (Machine: in out Parser_FSM;
                            Config : in     JSON_String_Buffer_Config)
   is begin
      Setup_Buffer (Target => Machine.Name_Output,   Config => Config);
      Setup_Buffer (Target => Machine.String_Output, Config => Config);
   end;
   
   ----------------------
   -- Configure_Limits --
   ----------------------
   
   procedure Configure_Limits (Machine: in out Parser_FSM;
                               Limits : in     Codec_Limits)
   is begin
      Machine.Limits         := Limits;
      Machine.Limits_Enabled := True;
   end;
   
   ---------------------
   -- Last_Indication --
   ---------------------
   
   function Last_Indication (Machine: Parser_FSM) return Operator_Indication is
     (Machine.Indication);
   
   -------------------
   -- Last_Position --
   -------------------
   
   function Last_Position (Machine: Parser_FSM) return Text_Position is
     (Machine.Position);
   
   ----------------------------
   -- Current_Structure_Kind --
   ----------------------------
   
   function Current_Structure_Kind (Machine: Parser_FSM) 
                                   return JSON_Structure_Kind is
     (Machine.Level_Kind);
   
   ----------
   -- Feed --
   ----------
   
   procedure Feed (Machine   : in out Parser_FSM;
                   Input     : in     Wide_Wide_Character;
                   Indication:    out Operator_Indication)
   is 
      procedure Advance_Line with Inline is
      begin
         if Machine.Position.Line < Positive'Last then
            Machine.Position.Line := Machine.Position.Line + 1;
            Machine.Position.Column := 0;
            Machine.Position.Overflow := False;
            -- If we could increment Line OK, since we're clearing
            -- Column, this means we are no longer in Overflow, if we
            -- were before (due Column overflowing)
         else
            Machine.Position.Overflow := True;
         end if;
      end;
   begin
      if Machine.Indication /= Ready then
         raise Program_Error;
      end if;
      
      if Machine.Last_Was_CR then
         -- CR is expected to be followed by LF. We don't have any real use
         -- of checking if it is or not, we will always count it as a new
         -- line.
         Advance_Line;
         Machine.Last_Was_CR := False;
      end if;
      
      if Input in JSON.Standards.Carriage_Return | JSON.Standards.Line_Feed
        and then
        Machine.State not in In_String | In_String_Escape | In_Unicode_Escape
      then
         if Input = JSON.Standards.Carriage_Return then
            Machine.Last_Was_CR := True;
         elsif Input = JSON.Standards.Line_Feed then
            Advance_Line;
         end if;
      end if;
      
      if Machine.Position.Column < Natural'Last then
         Machine.Position.Column := Machine.Position.Column + 1;
      else
         Machine.Position.Overflow := True;
      end if;
      
      Machine.Input := Input;
      Dispatch (Machine);
      Indication := Machine.Indication;
   exception
      when others =>
         Machine.State      := Halt;
         Machine.Indication := Invalid;
         Machine.Error      := Error_Unexpected_Exception'Access;
         Indication         := Machine.Indication;
   end;
   
   ---------------------
   -- Acknowledge_Pop --
   ---------------------
   
   procedure Acknowledge_Pop (Machine   : in out Parser_FSM;
                              Popped_To : in     JSON_Structure_Kind;
                              Indication:    out Operator_Indication)
   is begin
      Machine.Level_Kind := Popped_To;
      Machine.Value_Kind := Popped_To;
      Machine.State      := End_Value;
      Dispatch (Machine);
      Indication := Machine.Indication;
   exception
      when others =>
         Machine.State      := Halt;
         Machine.Indication := Invalid;
         Machine.Error      := Error_Unexpected_Exception'Access;
   end;
   
   ----------------------
   -- Acknowledge_Push --
   ----------------------
   
   procedure Acknowledge_Push (Machine   : in out Parser_FSM;
                               Indication:    out Operator_Indication) 
   is begin
      Machine.State      := After_Push;
      Machine.Indication := Ready;
      Indication         := Ready;
   end;
   
   -------------
   -- Outputs --
   -------------
   
   procedure Output_Name (Machine: in out Parser_FSM;
                          Target :    out JSON_String_Buffer)
   is begin
      Export (Source => Machine.Name_Output, Target => Target);
   end;
   
   function Output_Kind (Machine: Parser_FSM) return JSON_Value_Kind is
     (Machine.Value_Kind);
   
   procedure Output_Value (Machine: in out Parser_FSM;
                           Target :    out JSON_String_Buffer)
   is begin
      Export (Source => Machine.String_Output, Target => Target);
   end;
   
   function Output_Value (Machine: Parser_FSM) return Boolean is
     (Machine.Boolean_Output);
   
   function Output_Value (Machine: Parser_FSM) return JSON_Integer_Value is
     (Machine.Integer_Output);

   function Output_Value (Machine: Parser_FSM) return JSON_Float_Value is
     (Machine.Float_Output);
   
   ------------------------
   -- Acknowledge_Output --
   ------------------------
   
   procedure Acknowledge_Output (Machine   : in out Parser_FSM;
                                 Indication:    out Operator_Indication)
   is begin
      Machine.State := End_Value;
      Dispatch (Machine);
      Indication := Machine.Indication;
   exception
      when others =>
         Machine.State      := Halt;
         Machine.Indication := Invalid;
         Machine.Error      := Error_Unexpected_Exception'Access;
         Indication         := Machine.Indication;
   end;
   
   --------------------
   -- Invalid_Reason --
   --------------------
   
   function Invalid_Reason (Machine: Parser_FSM) return String is
     (Machine.Error.all);
   
   --------------------
   -- Emergency_Stop --
   --------------------
   
   Error_Emergency_Stop: aliased constant String
     := "Operator invoked Emergency Stop.";
   
   procedure Emergency_Stop (Machine: in out Parser_FSM) is
   begin
      if Machine.Indication in Invalid | Done then
         return;
      end if;
      
      Machine.State      := Halt;
      Machine.Indication := Invalid;
      Machine.Error      := Error_Emergency_Stop'Access;
   end;
   
   -----------
   -- Reset --
   -----------
   
   procedure Reset (Machine: in out Parser_FSM) is
   begin
      Machine.State        := Start_Value;
      Machine.Indication   := Ready;
      Machine.Error        := null;
      
      Machine.Position     := (Line => 1, Column => 0, Overflow => False);
      Machine.Level        := 0;
      Machine.Level_Kind   := JSON_Object;
      
      Machine.String_Total := 0;
      Machine.Entity_Total := 0;
   end Reset;
   
end JSON.Parser_Machine;
