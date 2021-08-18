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

with Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Maps;
with Ada.Characters.Conversions;

with Hex.Unsigned_16;

with AURA.JSON;

separate (JSON.Parser_Machine)
package body Transitions is
   
   use JSON.Standards;
   
   procedure Crash (Machine: in out Parser_FSM;
                    Error  : aliased not null access constant String)
   with Inline is
   begin
      Machine.State      := Halt;
      Machine.Indication := Invalid;
      Machine.Error      := Error;
   end Crash;
   
   
   function Output_Indication (Machine: Parser_FSM) return Operator_Indication 
   is (if Machine.Level_Kind = JSON_Object then 
          Output_Member else Output_Element);
   
   Entity_Limit_Exceeded_Error: aliased constant String
     := "The specified Entity Limit was exceeded.";
   
   function Entities_At_Limit (Machine: in out Parser_FSM) return Boolean is
     begin
     
      if Machine.Limits_Enabled 
        and then Machine.Entity_Total = Machine.Limits.Entity_Limit
      then
         Crash (Machine, Entity_Limit_Exceeded_Error'Access);
         return True;
      else
         return False;
      end if;
         
     end Entities_At_Limit;
   
   -- Checked any time State transitions to Output, and Indication transitions
   -- to Output_Member, Output_Array, or Push (also the places where the
   -- Entity_Total is incremented
   
   -----------------
   -- Start_Value --
   -----------------
   
   -- Errors
   
   Start_Value_Error_Initial: aliased constant String
     := "Expected '{' or '['";
   
   Start_Value_Error_Unexpected: aliased constant String 
     := "Expected the start of a value.";
   
   Start_Value_Error_Depth_Limited: aliased constant String
     := "Depth exceeds specified limit.";
   
   procedure Start_Value (Machine: in out Parser_FSM) is
      use Ada.Strings.Wide_Wide_Maps;
      
      Input: Wide_Wide_Character renames Machine.Input;
      State: Machine_State renames Machine.State;
      Indication: Operator_Indication renames Machine.Indication;
      
      function Depth_At_Limit return Boolean is
      begin
     
         if Machine.Limits_Enabled 
           and then Machine.Entity_Total = Machine.Limits.Depth_Limit
         then
            Crash (Machine, Start_Value_Error_Depth_Limited'Access);
            return True;
         else
            return False;
         end if;
         
     end Depth_At_Limit;
   
   -- Checked any time State transitions to Output, and Indication transitions
   -- to Output_Member, Output_Array, or Push (also the places where the
   -- Entity_Total is incremented
   begin
      pragma Assert (State = Start_Value);
      pragma Assert (Indication = Ready);
      
      -- We are expecting the start of any value.
      
      -- Skip any whitespace
      if Is_In (Element => Input, Set => Whitespace) then
         return;
      end if;
      
      if Machine.Level = 0
        and then not Is_In (Element => Input,
                            Set     => Nesting_Down)
      then
         -- If Level is zero, it means we haven't really "started" the JSON
         -- blob. We are stricting following the standard, and so all JSON
         -- blobs must be either a single object, or a single array.
         --
         -- This is also important for correctly initializing In_Name.
         
         Crash (Machine, Start_Value_Error_Initial'Access);
         return;
      end if;
         
      
      case Input is
         when Begin_Object =>
            -- Going into a new object
            
            if Entities_At_Limit (Machine) 
              or else Depth_At_Limit
            then 
               return; 
            end if;
            
            Machine.Entity_Total := Machine.Entity_Total + 1;
            Machine.Level := Machine.Level + 1;
            Machine.Level_Kind := JSON_Object;
            Machine.Value_Kind := JSON_Object;
            Machine.In_Name := True;
            
            State := Output;
            Indication := Push;
            
            
         when Begin_Array => 
            -- Going into a new array
            
            if Entities_At_Limit (Machine) 
              or else Depth_At_Limit
            then 
               return; 
            end if;
            
            Machine.Entity_Total := Machine.Entity_Total + 1;
            Machine.Level := Machine.Level + 1;
            Machine.Level_Kind := JSON_Array;
            Machine.Value_Kind := JSON_Array;
            Machine.In_Name := False;
            
            State := Output;
            Indication := Push;
            
         when Quotation_Mark =>
            -- Start of a string 
            State := Start_String;
            Start_String (Machine); -- tail feed
            
         when Literal_Null_Start =>
            Machine.CB_Level := Machine.Code_Buffer'First - 1;
            
            State := In_Null;
            In_Null (Machine); -- tail feed
            
         when Literal_True_Start =>
            Machine.CB_Level := Machine.Code_Buffer'First - 1;
            
            State := In_True;
            In_True (Machine); -- tail feed
            
         when Literal_False_Start =>
            Machine.CB_Level := Machine.Code_Buffer'First - 1;

            State := In_False;
            In_False (Machine); -- tail feed
            
         when others =>
            -- Only legal thing left is a numeric value. We do an explicit
            -- check here so that the error messages don't wrongly indicate
            -- that a numeric value was actually "expected"
            if Is_In (Element => Input,
                      Set     => Numeric_Begin_Set)
            then
               Machine.IB_Level := Machine.Integer_Buffer'First - 1;
               State := In_Integer;
               In_Integer (Machine); -- tail feed
            else
               -- Invalid
               Crash (Machine, Start_Value_Error_Unexpected'Access);
            end if;
      end case;
   end Start_Value;
   
   
   ------------------------
   -- Generic_In_Literal --
   ------------------------
   
   -- Generic code to handle all json literals (null, true, false)
   
   generic
      Check_Literal : in Wide_Wide_String;
      Mismatch_Error: not null access constant String;
      
      Kind: in JSON_Value_Kind;
      -- The kind of the expected literal. Expected to be JSON_Boolean or
      -- JSON_Null.
      
      Boolean_Result: in Boolean;
      -- Machine.Boolean_Output will be set to this value on match. For
      -- JSON_Null, the value set is immaterial (if the operator adheres to
      -- the preconditions, it won't be read)
   procedure Generic_In_Literal (Machine: in out Parser_FSM);
   
   procedure Generic_In_Literal (Machine: in out Parser_FSM) is
      Code_Buffer: Wide_Wide_String    renames Machine.Code_Buffer;
      CB_Level   : Natural             renames Machine.CB_Level;
      State      : Machine_State       renames Machine.State;
      Indication : Operator_Indication renames Machine.Indication;
   begin
      pragma Assert (Check_Literal'First = 1);
      pragma Assert (Check_Literal'Length <= Code_Buffer'Length);
      
      -- We are expecting to find the Check_Literal. This state is initially
      -- entered
      -- with a "tail feed", meaning we need to append input to the code buffer
      -- first, and then check the validity after
      
      CB_Level := CB_Level + 1;
      Code_Buffer(CB_Level) := Machine.Input;
      
      -- Note that this can never overflow because we are always checking that
      -- we're on the way to having "null" in the buffer. Since we stop as soon
      -- as we get that or we get a wrong input.
      --
      -- Of course we can rely on the Ada checks to catch that if we are
      -- somehow wrong
      
      
      if Code_Buffer(1 .. CB_Level) = Check_Literal(1 .. CB_Level) then
         -- Looking good
         
         if CB_Level = Check_Literal'Last then
            -- Full match
            if Entities_At_Limit (Machine) then return; end if; -- Crash
            Machine.Entity_Total := Machine.Entity_Total + 1;
            
            -- Setup the output
            
            Machine.Value_Kind := Kind;
            Machine.Boolean_Output := Boolean_Result;
            
            State := Output;
            Indication := Output_Indication (Machine);
            
         else
            -- Still working on it
            Indication := Ready;
         end if;
      else
         -- No-longer a match (therefore invalid)
         Crash (Machine, Mismatch_Error);
      end if;
   end Generic_In_Literal;
   
   -------------
   -- In_Null --
   -------------
   
   -- Errors
   In_Null_Error_Mismatch: aliased constant String
     := "Expected literal 'null'";
   
   procedure In_Null_Instance is new Generic_In_Literal
     (Check_Literal  => Literal_Null,
      Mismatch_Error => In_Null_Error_Mismatch'Access,
      Kind           => JSON_Null,
      Boolean_Result => False);
   
   procedure In_Null (Machine: in out Parser_FSM) renames In_Null_Instance;
   
   -------------
   -- In_True --
   -------------
   
   -- Errors
   In_True_Error_Mismatch: aliased constant String
     := "Expected literal 'true'";
   
   procedure In_True_Instance is new Generic_In_Literal
     (Check_Literal  => Literal_True,
      Mismatch_Error => In_True_Error_Mismatch'Access,
      Kind           => JSON_Boolean,
      Boolean_Result => True);
   
   procedure In_True (Machine: in out Parser_FSM) renames In_True_Instance;
   
   --------------
   -- In_False --
   --------------
   
   -- Errors
   In_False_Error_Mismatch: aliased constant String
     := "Expected literal 'false'";
   
   procedure In_False_Instance is new Generic_In_Literal
     (Check_Literal  => Literal_False,
      Mismatch_Error => In_False_Error_Mismatch'Access,
      Kind           => JSON_Boolean,
      Boolean_Result => False);
   
   procedure In_False (Machine: in out Parser_FSM) renames In_False_Instance;
   
   
   ----------------
   -- In_Integer --
   ----------------
   
   In_Integer_Error_Bad_Numeric: aliased constant String
     := "Expected a valid numeric digit, decimal point, or exponent.";
   
   In_Integer_Error_Leading_Zeros: aliased constant String
     := "Leading zeros are not allowed.";
   
   In_Integer_Error_Too_Large: aliased constant String
     := "Number is too large.";
   
   In_Integer_Error_Conversion_Fail: aliased constant String
     := "Decoded number could not be interpreted (Ada). " 
       & "Number might be out of range.";
   
   ----------------------------------------------------------------------
   
   generic
      type Target_Type is private;
      
      with function Wide_Wide_Value (Image: Wide_Wide_String) 
                                    return Target_Type;
      -- Corresponds with the scalar S'Wide_Wide_Value
      
      Kind   : in     JSON_Value_Kind;
      Machine: in out Parser_FSM;
      Buffer : in     Wide_Wide_String;
      Target : in out Target_Type;
   procedure Generic_Produce_Scalar_Output;
   
   procedure Generic_Produce_Scalar_Output is
   begin
      -- Check the entity limit first
      if Entities_At_Limit (Machine) then return; end if;
      Machine.Entity_Total := Machine.Entity_Total + 1;
      
      -- Input should contain a valid Ada image
      Machine.Value_Kind    := Kind;
      Target                := Wide_Wide_Value (Buffer);
      Machine.State         := Output;
      Machine.Indication    := Output_Indication (Machine);
   exception
      when others =>
         Crash (Machine, In_Integer_Error_Conversion_Fail'Access);
   end Generic_Produce_Scalar_Output;
   
   ----------------------------------------------------------------------
   
   procedure Produce_Integer_Output (Machine: in out Parser_FSM) with 
     Pre => Machine.State in In_Integer | In_Integer_Exponent
            and Machine.IB_Level >= Machine.Integer_Buffer'First
   is 
      procedure Execute is new Generic_Produce_Scalar_Output
        (Target_Type     => JSON_Integer_Value,
         Wide_Wide_Value => JSON_Integer_value'Wide_Wide_Value,
         Kind            => JSON_Integer,
         Machine         => Machine,
         Buffer          => Machine.Integer_Buffer (1 .. Machine.IB_Level),
         Target          => Machine.Integer_Output);
   begin
      Execute;
   end Produce_Integer_Output;
   
   ----------------------------------------------------------------------
   
   procedure In_Integer (Machine: in out Parser_FSM) is
      use Ada.Strings.Wide_Wide_Maps;
      
      IB_Level      : Natural             renames Machine.IB_Level;
      Integer_Buffer: Wide_Wide_String    renames Machine.Integer_Buffer;
      Input         : Wide_Wide_Character renames Machine.Input;
      State         : Machine_State       renames Machine.State;
      Indication    : Operator_Indication renames Machine.Indication;
      
   begin
      -- In_Integer is entered through a tail feed as soon as we see a valid
      -- start to a json numerical value ('-' or 0-9). Until we see a decimal
      -- we assume that we are looking at an integer value, and therefore are
      -- filling the Integer_Buffer with the literal, and ensuring that it is
      -- always a valid integer until we reach the end or see something we
      -- don't like.
      
      -- Check for validity of this one before appending. We always want to
      -- stop parsing as soon as we know it is wrong.
      
      -- Note that the first character has already been checked inside Start_Value.
      
      if IB_Level >= Integer_Buffer'First then
         if Is_In (Element => Input, Set => Number_Termination_Set) then
            -- Note that we don't check this for IB_Level = 1, since this already
            -- checked via Start_Value.
            Produce_Integer_Output (Machine);
            return;
         elsif not Is_In (Element => Input, Set => Numeric_Continue_Set) then
            Crash (Machine, In_Integer_Error_Bad_Numeric'Access);
            return;
         end if;
      end if;
      
      
      pragma Assert (not Is_In (Element => Input, Set => Whitespace));
      

      
      -- Try to make room for the new character
      IB_Level := IB_Level + 1;
      
      if IB_Level > Integer_Buffer'Last then
         Crash (Machine, In_Integer_Error_Too_Large'Access);
         return;
      end if;
      
      -- Load in the Input
      Integer_Buffer(IB_Level) := Input;
      
      
      -- Check for leading zeros
      
      if IB_Level = 2 then
         if Integer_Buffer(1) = Num_Zero 
           and then Is_In (Element => Integer_Buffer(2), Set => Digit_Set)
         then
            Crash (Machine, In_Integer_Error_Leading_Zeros'Access);
            return;
         end if;
      elsif IB_Level = 3 then
         if Integer_Buffer(1) = Minus_Sign 
           and then Integer_Buffer(2) = Num_Zero
           and then Is_In (Element => Integer_Buffer(3), Set => Digit_Set)
         then
            Crash (Machine, In_Integer_Error_Leading_Zeros'Access);
            return;
         end if;
      end if;
      
      -- Final checks and transitions
      
      case Input is
         
         when Decimal_Point =>
            -- Transition to a float value
            if IB_Level > Machine.Float_Buffer'Last then
               -- We couldn't fit the integer into the float buffer at all!
               Crash (Machine, In_Integer_Error_Too_Large'Access);
               return;
            end if;
            
            Machine.FB_Level := IB_Level;
            Machine.Float_Buffer(1 .. IB_Level) 
              := Integer_Buffer(1 .. IB_Level);
            
            State := In_Float;
            
         when Exponent_Lower =>
            -- Push to upercase and then transition
            Integer_Buffer(IB_Level) := Exponent_Upper;
            State := In_Integer_Exponent;
            
         when Exponent_Upper =>
            -- No need to modify
               State := In_Integer_Exponent;
            
         when others =>
            pragma Assert (if IB_Level = 1 then 
                              Is_In (Element => Input, 
                                     Set     => Numeric_Begin_Set)
                           else
                              Is_In (Element => Input,
                                     Set     => Numeric_Continue_Set));
            Indication := Ready;
            
      end case;
   end In_Integer;
   
   --------------
   -- In_Float --
   --------------
   
   In_Float_Error_Unexpected: aliased constant String
     :=  "Unexpected character in number fractional part. " 
       & "Shall be a digit or exponent marker.";
   
   In_Float_Error_Digit_Start: aliased constant String
     := "The fractional part must have at least done digit.";
   
   ----------------------------------------------------------------------
   
   
   
   procedure Produce_Float_Output (Machine: in out Parser_FSM) with 
     Pre => Machine.State in In_Float | In_Float_Exponent
            and Machine.FB_Level >= Machine.Float_Buffer'First
   is
      procedure Execute is new Generic_Produce_Scalar_Output
        (Target_Type     => JSON_Float_Value,
         Wide_Wide_Value => JSON_Float_Value'Wide_Wide_Value,
         Kind            => JSON_Float,
         Machine         => Machine,
         Buffer          => Machine.Float_Buffer (1 .. Machine.FB_Level),
         Target          => Machine.Float_Output);
   begin
      Execute;
   end Produce_Float_Output;
   
   ----------------------------------------------------------------------
   
   procedure In_Float (Machine: in out Parser_FSM) is
      use Ada.Strings.Wide_Wide_Maps;
      
      FB_Level    : Natural             renames Machine.FB_Level;
      Float_Buffer: Wide_Wide_String    renames Machine.Float_Buffer;
      Input       : Wide_Wide_Character renames Machine.Input;
      State       : Machine_State       renames Machine.State;
      Indication  : Operator_Indication renames Machine.Indication;
      
      Truncate_Floats: Boolean renames AURA.JSON.Configuration.Truncate_Floats;
      
   begin
      -- So we've been passed a string from In_Integer that includes the
      -- "[minus] int" part plus the decimal point, which means we are expecting
      -- to see only DIGITs, until whitespace or an exponent marker.
      --
      -- Of course, for a fractional part, "leading zeros" are fine. We wouldn't
      -- want to make "0.000001" illegal! Technically we should probably
      -- disallow "1.0000000" but that seems really obsurd. There is no attack
      -- surface here since Float_Buffer is a finite size anyways.
      
      -- Now we assume a digit but keep an eye-out for +/- (only allowed
      -- at Decimal_Position + 1)
      
      -- The fraction component must always start with a digit
      if FB_Level = 0
        and then not Is_In (Element => Input, Set => Digit_Set)
      then
         Crash (Machine, In_Float_Error_Digit_Start'Access);
         
      elsif Is_In (Element => Input, Set => Number_Termination_Set) then
         Produce_Float_Output (Machine);
         return;
      end if;   
      
      -- Try to add. If the package configuration is set to truncate floats,
      -- we will simply ignore any digits past the max
      
      FB_Level := FB_Level + 1;
      
      if FB_Level > Float_Buffer'Last then
         if Truncate_Floats then
            FB_Level := Float_Buffer'Last;
         else
            Crash (Machine, In_Integer_Error_Too_Large'Access);
            return;
         end if;
         
      else
         Float_Buffer(FB_Level) := Input; 
      end if;
      

      
      Indication := Ready;
      -- All transitions from here-on, besides crashes, should indicate
      -- Ready
      
      if Is_In (Element => Input, Set => Digit_Set) then
         -- Just another number. No transition
         return;
      end if;
      
      -- Anything else should be an exponent.
      

      
      case Input is
         when Exponent_Lower | Exponent_Upper =>
            
            -- If truncation is happening 'E' needs to over-write the last
            -- digit of the fraction digit, which means that must also be
            -- preceeded by a digit, since we need at least one digit in the
            -- fractional part
            
            if Truncate_Floats 
              and then FB_Level = Float_Buffer'Last
              and then not Is_In (Element => Float_Buffer(FB_Level - 1),
                                  Set     => Digit_Set)
            then
               Crash (Machine, In_Integer_Error_Too_Large'Access);
               return;
            end if;
            
            -- Ensure upercase 'E' and then transition
            Float_Buffer(FB_Level) := Exponent_Upper;
            State := In_Float_Exponent;
            
         when others =>
            Crash (Machine, In_Float_Error_Unexpected'Access);
      end case;
   end In_Float;
   
   
   -------------------------
   -- In_Integer_Exponent --
   -------------------------
   
   In_Exponent_Error_No_Digits: aliased constant String
     := "The exponent part must contain at least one digit.";
   
   In_Exponent_Error_Bad_Polrity_Symbol: aliased constant String
     := "+/- must only occur immediatly after e/E, and only once.";
   
   In_Exponent_Error_Leading_Zeros: aliased constant String
     := "Exponent should not have leading zeros.";
     
   In_Exponent_Error_Unexpected_Input: aliased constant String
     := "Unexpected character in exponent portion.";
   
   ----------------------------------------------------------------------
   
   generic
      Machine   : in out Parser_FSM;
      
      Buffer    : in out Wide_Wide_String;
      Level     : in out Natural;
      
      Truncate_Float: in Boolean;
      
      with procedure Produce_Output (Machine: in out Parser_FSM);
      with procedure Test_Negative_Exponent is null;
   
   procedure Generic_In_Exponent;
   
   procedure Generic_In_Exponent is
      use Ada.Strings.Wide_Wide_Maps;
      
      Input       : Wide_Wide_Character renames Machine.Input;
      State       : Machine_State       renames Machine.State;
      Indication  : Operator_Indication renames Machine.Indication;
      
      Crashed     : Boolean := False;
      
      procedure Float_Writeback is
         -- When truncation is enabled, and we are at the limit of the
         -- buffer, we need to shift the exponent left in the buffer in such a
         -- way that ensures we are only truncating the frational part
         
         E_Mark: Natural := Buffer'First - 1;
      begin
         pragma Assert (Level = Buffer'Last);
         
         for I in reverse Buffer'First .. Level loop
            if Buffer(I) = Exponent_Upper then
               E_Mark := I;
               exit;
            end if;
         end loop;
         
         if E_Mark not in Buffer'First + 2 .. Buffer'Last - 1 
           -- We expect, at a minumum '.n' infront of the exponent,
           -- so we always need at least two digits preceeding the exponent
           -- for this writeback
           
           or else not Is_In (Element => Buffer(E_Mark - 2),
                              Set     => Digit_Set)
           
           or else not Is_In (Element => Buffer(E_Mark - 1),
                              Set     => Digit_Set)
         then
            Crash (Machine, In_Integer_Error_Too_Large'Access);
            Crashed := True;
            return;
         end if;
         
         Buffer(E_Mark - 1) := Exponent_Upper;
         Buffer(E_Mark) := Buffer(E_Mark + 1);
         
      end Float_Writeback;
      
      procedure Append_Input is
      begin
         Level := Level + 1;
         if Level > Buffer'Last then
            if Truncate_Float then
               Level := Buffer'Last;
               Float_Writeback;
               if Crashed = True then return; end if;
               
            else
               Crash (Machine, In_Integer_Error_Too_Large'Access);
               Crashed := True;
               return;
            end if;
         end if;
         
         Indication := Ready;
         Buffer(Level) := Input;
      end Append_Input;
      
   begin
      
      -- We arrive in In_Exponent after In_Integer/Float has processed the
      -- 'e'. So we are looking at possibly a '-' or '+', and some series of
      -- digits. We need to have at least one digit before a termination is
      -- valid
      
      if Is_In (Element => Input, Set => Number_Termination_Set) then
         
         -- The previously entered value must be a digit for this to be
         -- valid. We'll alow an exponent of zero, not like it really
         -- matters
         
         if Is_In (Element => Buffer(Level), Set => Digit_Set) then
            Produce_Output (Machine);
            return;
         else
            Crash (Machine, In_Exponent_Error_No_Digits'Access);
            return;
         end if;
         
      elsif Is_In (Element => Input, Set => Digit_Set) then
         -- Ignore "leading zeros" in exponent part unless Buffer(Level)
         -- is either an exponent or a +/- sign. This ensures that we
         -- have at least one zero if the exponent is all zeros
         if Input = Num_Zero
           and then Buffer(Level) not in 
           Exponent_Upper | Minus_Sign | Plus_Sign
           
           and then Buffer(Level) /= Num_Zero
         then
            Append_Input;
            if Crashed then return; end if;
         end if;
         
      elsif Input in Minus_Sign | Plus_Sign then
         -- The previous value must be an exponent symbol for a '-' or '+'. 
         -- to be valid. Note that the In_Integer/Float state always forces the
         -- exponent symbol to be upper-case (for Ada compatibility)
         
         if Buffer(Level) /= Exponent_Upper then
            Crash (Machine, In_Exponent_Error_Bad_Polrity_Symbol'Access);
         else
            Append_Input;
            if Crashed then return; end if;
            
            Test_Negative_Exponent;
            -- For negative exponents on Integer values, we will want
            -- to convert it to a float. This is handled in In_Integer_Exponent
            -- speficially, via this optional formal procedure
         end if;
         
      else
         -- Anything else is wrong
         Crash (Machine, In_Exponent_Error_Unexpected_Input'Access);
      end if;
      
   end Generic_In_Exponent;
   
   ----------------------------------------------------------------------
   
   procedure In_Integer_Exponent (Machine: in out Parser_FSM) is
      
      procedure Expand_Then_Produce (Machine: in out Parser_FSM) is
         use Ada.Strings;
         package WWF renames Ada.Strings.Wide_Wide_Fixed;
         
         Buffer: Wide_Wide_String renames Machine.Integer_Buffer;
         Level : Natural          renames Machine.IB_Level;
         
         E_Start: constant Natural 
           := WWF.Index (Source => Buffer(Buffer'First .. Level),
                         Pattern => Wide_Wide_String'(1 => Exponent_Upper),
                         Going => Backward);
         
         pragma Assert (E_Start in Buffer'First + 1 .. Level - 1);
         
         Zeros_Value: Wide_Wide_String renames Buffer (E_Start + 1 .. Level);
         Zeros: Natural;
                        
      begin
         -- Though the Ada RM suggest that, e.g. 1E10 is a valid integer
         -- literal (and thus input to 'Value), GNAT does not think so.
         -- for maximum compatibility, we expand the EXX into the corresponding
         -- number of zeros that are appended to the value before the 'E',
         -- and replace the entire exponent portion with that value. If
         -- it exceeds the buffer size, then we can safely assume that the
         -- value is too large anyways
         
         Zeros := Natural'Wide_Wide_Value (Zeros_Value);
         
         if Zeros = 0 then
            Level := E_Start - 1;
            Produce_Integer_Output (Machine);
            return;
         end if;
         
         Level := E_Start + Zeros - 1;
            
         if Level > Buffer'Last then
            Crash (Machine, In_Integer_Error_Too_Large'Access);
            return;
         else
            Buffer (E_Start .. Level) := (others => Num_Zero);
            Produce_Integer_Output (Machine);
         end if;
         
      exception
         when others =>

            -- This should not really be possible
            Crash (Machine, In_Integer_Error_Conversion_Fail'Access);
      end;
      
      procedure Negative_Exponent_Conversion is
         Integer_Buffer: Wide_Wide_String renames Machine.Integer_Buffer;
         Float_Buffer  : Wide_Wide_String renames Machine.Float_Buffer;
         IB_Level      : Natural          renames Machine.IB_Level;
         FB_Level      : Natural          renames Machine.FB_Level;
      begin
         if Machine.Input /= Minus_Sign then return; end if;
         
         -- We have a negative exponent, which has already been entered into
         -- the buffer. We will adjust the buffer by appending a ".0" before
         -- the exponent value to make it a float, and then transition into
         -- a float state for the next input
         
         -- Make sure we can add ".0" and fit it all in the float buffer
         
         FB_Level := IB_Level + 2;  -- Integer_Buffer + ".0"
         
         if FB_Level > Float_Buffer'Last then
            -- Not enough room in the float!
            Crash (Machine, In_Integer_Error_Too_Large'Access);
            return;
         end if;
         
         -- Right now the Integer_Buffer contains: "[digits]E-". We want
         -- the float buffer to contain "[digits].0E-". We know that the
         -- parser would not have allowed the '-' to not be after the 'E',
         -- so we know that the 'E' is at Level - 1, and we also know there
         -- is at least one digit before the 'E'. Hence the current buffer
         -- must be at least 3 characters long
         
         pragma Assert (IB_Level >= 3);
         
         -- Recall: "[digits]E-"
         --          ^^^^^^^^||--- Level
         --                 ||---- Level - 1
         --                 |----- Level - 2
         

         
         Float_Buffer (1 .. FB_Level) 
           := Integer_Buffer (1 .. IB_Level - 2) & ".0E-";

         pragma Assert (Float_Buffer(FB_Level) = Minus_Sign);
         
         Machine.State := In_Float_Exponent;
         Machine.Indication := Ready;
         
      end Negative_Exponent_Conversion;
      
      procedure Execute is new Generic_In_Exponent
        (Machine                => Machine,
         Buffer                 => Machine.Integer_Buffer,
         Level                  => Machine.IB_Level,
         Truncate_Float         => False,
         Produce_Output         => Expand_Then_Produce,
         Test_Negative_Exponent => Negative_Exponent_Conversion);
   begin
      Execute;
   end In_Integer_Exponent;
   
   -----------------------
   -- In_Float_Exponent --
   -----------------------
   
   procedure In_Float_Exponent (Machine: in out Parser_FSM) is
      procedure Execute is new Generic_In_Exponent
        (Machine        => Machine,
         Buffer         => Machine.Float_Buffer,
         Level          => Machine.FB_Level,
         Truncate_Float => AURA.JSON.Configuration.Truncate_Floats,
         Produce_Output => Produce_Float_Output);
   begin
      Execute;
   end In_Float_Exponent;
   
   ------------------
   -- Start_String --
   ------------------
   
   Start_String_Error_Unexpected: aliased constant String
     := "Expected a string ('""').";
   
   procedure Start_String (Machine: in out Parser_FSM) is
      use Ada.Strings.Wide_Wide_Maps;
   begin
      if Is_In (Element => Machine.Input, Set => Whitespace) then
         Machine.Indication := Ready;
      elsif Machine.Input = Quotation_Mark then
         Machine.String_Total := 0;
         Machine.SB_Level := Machine.Scratch_Buffer'First - 1;
         
         if Machine.In_Name then
            Clear (Machine.Name_Output);
         else
            Clear (Machine.String_Output);
         end if;
         
         Machine.State := In_String;
         Machine.Indication := Ready;
      else
         Crash (Machine, Start_String_Error_Unexpected'Access);
      end if;
   end Start_String;
   
   ---------------
   -- In_String --
   ---------------
   
   In_String_Error_Could_Not_Append: aliased constant String
     :=  "Could not append scratch buffer chunk to string buffer. "
       & "String buffer might not be large enough.";
   
   In_String_Error_Limit: aliased constant String
     := "String length exceeds specified limit.";
   
   ----------------------------------------------------------------------
   
   procedure In_String (Machine: in out Parser_FSM) is
      Input     : Wide_Wide_Character   renames Machine.Input;
      State     : Machine_State         renames Machine.State;
      Indication: Operator_Indication   renames Machine.Indication;
      
      Scratch_Buffer: JSON_String_Value renames Machine.Scratch_Buffer;
      SB_Level      : Natural           renames Machine.SB_Level;
      String_Total  : Natural           renames Machine.String_Total;
      
      function Limits_Exceeded return Boolean is
      begin
         if Machine.Limits_Enabled 
           and then String_Total = Machine.Limits.String_Limit
         then
            Crash (Machine, In_String_Error_Limit'Access);
            return True;
         else
            return False;
         end if;
      end;

      
      procedure Flush_Scratch is
      begin
         if SB_Level = 0 then return; end if;
         
         if Machine.In_Name then
            Append (Target => Machine.Name_Output,
                    Source => Scratch_Buffer(Scratch_Buffer'First .. SB_Level));
         else
            Append (Target => Machine.String_Output,
                    Source => Scratch_Buffer(Scratch_Buffer'First .. SB_Level));
         end if;
         
         SB_Level := Scratch_Buffer'First - 1;
      end Flush_Scratch;
      
      function Try_Flush_If_Needed (Force: Boolean := False) return Boolean is
      begin
         if SB_Level = Scratch_Buffer'Last or else Force then
            begin
               Flush_Scratch;
            exception
               when others =>
                  Crash (Machine, In_String_Error_Could_Not_Append'Access);
                  return False;
            end;
            
            SB_Level := Scratch_Buffer'First - 1;
            return True;
            
         else
            return True;
         end if;
      end Try_Flush_If_Needed;
      
   begin
      -- We are now in a String. Our main job is to fill up the scratch buffer.
      -- When the scratch buffer fills-up we push it into the correct output
      -- buffer.
      --
      -- If we encounter a '\', then we need to handle an escape sequence
      -- (in another state). Otherwise we wait to see a '"' to signal
      -- termination of the string.
      --
      -- Everything else goes.
      
      case Input is
         
         when Quotation_Mark =>
            -- End of string! flush and transition
            if not Try_Flush_If_Needed (Force => True) then return; end if;
            
            if Machine.In_Name then
               State := After_Name;
               Indication := Ready;
            else
               if Entities_At_Limit (Machine) then return; end if;
               Machine.Entity_Total := Machine.Entity_Total + 1;
               Machine.Value_Kind := JSON_String;
               State := Output;
               Indication := Output_Indication (Machine);
            end if;
            
         when Escape_Symbol =>
            -- Escape sequence! Obviously the '\' can be discarded.
            -- But we will promise the following state(s) that there
            -- will be room in the scratch buffer, and that the limit
            -- wont be exceeded
            
            if not Try_Flush_If_Needed then return; end if;
            
            State := In_String_Escape;
            Indication := Ready;
            
         when others =>
            -- Check if there is room
            if Limits_Exceeded then return; end if;
            
            -- Attempt to append to the flush the scratch buffer if it is full
            if not Try_Flush_If_Needed then return; end if;
            
            SB_Level := SB_Level + 1;
            Scratch_Buffer(SB_Level) := Input;
            String_Total := String_Total + 1;
            Indication := Ready;
      end case;
   end In_String;
   
   ----------------------
   -- In_String_Escape --
   ----------------------
   
   In_String_Escape_Invalid_Code: aliased constant String
     := "Invalid escape sequence. Expected one of: "
       & "\"", \\, \/, \b, \f, \n, \r, \t or \u.";
   
   procedure In_String_Escape (Machine: in out Parser_FSM) is
      use Ada.Strings.Wide_Wide_Maps;
      
      Input     : Wide_Wide_Character   renames Machine.Input;
      State     : Machine_State         renames Machine.State;
      Indication: Operator_Indication   renames Machine.Indication;
      
      Scratch_Buffer: JSON_String_Value renames Machine.Scratch_Buffer;
      SB_Level      : Natural           renames Machine.SB_Level;
      String_Total  : Natural           renames Machine.String_Total;
      
   begin
      -- This state handles all non-unicode escape sequences, which means this
      -- state is strictly single-cycle!
      
      -- Note that In_String has already ensured that there is enough room in
      -- the scratch buffer for whatever character is escaped.
      

      if not Is_In (Element => Input, Set => Escape_Code_Set) then
         -- Intercept any invalid codes first
         Crash(Machine, In_String_Escape_Invalid_Code'Access);
         
      elsif Input = Universal_Code then
         -- Intercept the unicode escape case next.
         
         -- We discard the 'u', and the promise of space for a new character is
         -- passed-on
         
         Machine.CB_Level := 0;
         State := In_Unicode_Escape;
         Indication := Ready;
         
      else
         SB_Level := SB_Level + 1;
         String_Total := String_Total + 1;
         -- Note that the limit was already checked in In_String.
         -- We could have done the incrementing there too, but it would be bad
         -- form.
         
         Scratch_Buffer(SB_Level) := Value (Map     => Escape_Code_Map,
                                            Element => Input);
         -- Note that this should not possibly raise an exception due to the
         -- check that Input is in the Escape_Code_Set
         
         pragma Assert (Scratch_Buffer(SB_Level) /= Universal_Code);
         -- This should be impossible, though the mapping does "allow" it.
         
         State := In_String;
         Indication := Ready;
         -- Ready for the next character in the string!
         
      end if;
   end In_String_Escape;
   
   -----------------------
   -- In_Unicode_Escape --
   -----------------------
   
   In_Unicode_Escape_Error_Format: aliased constant String
     := "Unicode escape sequences must have exactly 4 hex digits.";
   
   In_Unicode_Escape_Error_Invalid: aliased constant String
     := "Unicode escape sequence did not encode a valid character.";
   
   procedure In_Unicode_Escape (Machine: in out Parser_FSM) is
      
      use Ada.Strings.Wide_Wide_Maps;
      
      Input         : Wide_Wide_Character   renames Machine.Input;
      State         : Machine_State         renames Machine.State;
      Indication    : Operator_Indication   renames Machine.Indication;
      
      Scratch_Buffer: JSON_String_Value     renames Machine.Scratch_Buffer;
      SB_Level      : Natural               renames Machine.SB_Level;
      String_Total  : Natural               renames Machine.String_Total;
      
      Code_Buffer   : Wide_Wide_String      renames Machine.Code_Buffer;
      CB_Level      : Natural               renames Machine.CB_Level;
      
   begin
      -- We are strictly expecting a four-digit hex value in this stage.
      -- Nothing else is acceptible.
      
      if not Is_In (Element => Input, Set => Escape_Hex_Set) then
         Crash (Machine, In_Unicode_Escape_Error_Format'Access);
         return;
      end if;
      
      CB_Level := CB_Level + 1;
      pragma Assert (CB_Level <= 4);
      
      Code_Buffer(CB_Level) := Input;
      
      if CB_Level = 4 then
         -- It's showtime
         declare
            use Hex.Unsigned_16;
            use Ada.Characters.Conversions;
            Hex_String: constant String := To_String (Code_Buffer(1 .. 4));
         begin
            SB_Level := SB_Level + 1;
            String_Total := String_Total + 1;
            -- Note that the limit was already checked in In_String.
            
            Scratch_Buffer(SB_Level) 
              := Wide_Wide_Character'Val(Decode(Hex_String));
            
            State := In_String;  -- Done
         end;
      end if;         

      Indication := Ready;
      
   exception
      when others =>
         Crash (Machine, In_Unicode_Escape_Error_Invalid'Access);
   end In_Unicode_Escape;
   
   ----------------
   -- After_Name --
   ----------------
   
   After_Name_Error_Unexpected: aliased constant String
     := "Expected ':' after member name";
   
   procedure After_Name (Machine: in out Parser_FSM) is
      use Ada.Strings.Wide_Wide_Maps;
      
      Input: Wide_Wide_Character renames Machine.Input;
      State: Machine_State renames Machine.State;
      Indication: Operator_Indication renames Machine.Indication;
   begin
      -- Pretty simple, skip any whitespace, after which we better find
      -- Name_Separator (':')
      
      if Input = Name_Separator then
         Machine.In_Name := False;
         State := Start_Value;
         Indication := Ready;
      elsif Is_In (Element => Input, Set => Whitespace) then
         Indication := Ready;
      else
         Crash (Machine, After_Name_Error_Unexpected'Access);
      end if;
   end After_Name;
   
   ---------------
   -- End_Value --
   ---------------
   
   procedure End_Value (Machine: in out Parser_FSM) is
   begin
      -- End_Value is entered directly from Acknowledge_Output, and
      -- Popped_To_Kind and is used to decide if a tail feed is needed instead
      -- of a new feed for the next state of After_Value.
      
      Machine.State := After_Value;
      Machine.Indication := Ready;
      
      -- Case statement to make sure we always get all the values
      case Machine.Value_Kind is
         when JSON_Integer | JSON_Float =>
            -- Tail feed
            After_Value (Machine);
            
         when JSON_Object | JSON_Array | 
           JSON_String | JSON_Boolean | JSON_Null =>
            -- New feed
            null;
      end case;
      
   end End_Value;
   
   ----------------
   -- After_Push --
   ----------------
   
   procedure After_Push (Machine: in out Parser_FSM) is
      use Ada.Strings.Wide_Wide_Maps;
   begin
      -- We enter this state after the operator has acknowledged a Push,
      -- and has fed fresh input on a subsequent feed. We are expecting
      -- Input to be either a valid Pop condition for After_Value ('}' or ']'),
      -- or it should be a valid input for Start_Value. So therefore we will
      -- tail feed to After_Push if we know it is right for that, or
      -- Start_Value for everything else
      
      -- First skip any whitespace.
      if Is_In (Element => Machine.Input, Set => Whitespace) then
         return;
      end if;
      
      if Is_In (Element => Machine.Input, Set => Nesting_Up) then
         Machine.State := After_Value;
         After_Value (Machine); -- Tail feed
      else
         Machine.State := Start_Value;
         Start_Value (Machine); -- Tail feed
      end if;
      
   end After_Push;
   
   -----------------
   -- After_Value --
   -----------------
   
   After_Value_Error_Unexpected: aliased constant String
     := "Expected a value-terminating character (',', ']', or '}').";
   
   procedure After_Value (Machine: in out Parser_FSM) is
      use Ada.Strings.Wide_Wide_Maps;
      
      Input     : Wide_Wide_Character renames Machine.Input;
      State     : Machine_State       renames Machine.State;
      Indication: Operator_Indication renames Machine.Indication;
      Level     : Positive            renames Machine.Level;
   begin
      -- This state is entered from End_Value, which ensures that Input is
      -- one character beyond the last character that constituted the output,
      -- and after skipping whitespace, should represent a valid character
      -- that determines how a value actually ends (end of structure, or end
      -- of member/element of that structure)
      
      -- We're expecting one of two things here (after skipping whitespace).
      -- 1. A Structural close (} or ])
      -- 2. An indication of a new value (',')
      --
      -- Anything else is wrong.
      
      if Input = Value_Separator then
         -- This this case, if we are currently inside of an object,
         -- the next value must be a string (a name).
         Machine.In_Name := (Machine.Level_Kind = JSON_Object);
         State := Start_Value;
         Indication := Ready;
      elsif Is_In (Element => Input, Set => Whitespace) then
         Indication := Ready;
      elsif Is_In (Element => Input, Set => Nesting_Up) then
         -- See if this is "it"
         if Level = 1 then
            State := Halt;
            Indication := Done;
         else
            Level := Level - 1;
            State := Output;
            Indication := Pop;
            
            -- Note that Value_Kind is set with Popped_To_Kind, since
            -- this output is also partly an input. Setting Value_Kind
            -- here, though harmless, is not really "correct"
         end if;
      else
         -- Invalid
         Crash (Machine, After_Value_Error_Unexpected'Access);
      end if;
      
   end After_Value;
   
end Transitions;
