------------------------------------------------------------------------------
--                                                                          --
--                         JSON Parser/Constructor                          --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2020-2021, ANNEXI-STRAYLINE Trans-Human Ltd.              --
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

-- This package implements the JSON parser finite state machine used by
-- both codec deserialization

private generic
   
   type JSON_String_Buffer is limited private;
   type JSON_String_Buffer_Config is private;
   
   -- A buffer in which to store member names and string values during
   -- parsing.
   --
   -- This is also the value that is Export'ed for string outputs and
   -- member names.
   --
   -- If an exception is raised when executing any of the following operations,
   -- the parser halts with an appropriate error condition.
   
   with procedure Setup_Buffer (Target: in out JSON_String_Buffer;
                                Config: in     JSON_String_Buffer_Config)
     is null;
   
   -- Setup_Buffer will be invoked by the Parser_FSM's Setup_Buffers operation,
   -- on each internal buffer. Config will be transfered from Setup_Buffers.
   -- This is specifically to allow the Unbounded_Codec's custom Slab_Strings.
   
   with procedure Clear (Target: in out JSON_String_Buffer);
   
   with procedure Append (Target: in out JSON_String_Buffer;
                          Source: in     JSON_String_Value);
   
   with procedure Export (Source: in out JSON_String_Buffer;
                          Target:    out JSON_String_Buffer);
   
   -- Export shall copy the contents of Source to Target, and may or may not
   -- Clear Source. Clear will be invoked under the assumption that Export
   -- Does NOT clear Source.
   
   String_Chunk_Size: in Positive := 100;
   
   -- The size of an internal state buffer that is filled before being
   -- appended to a JSON_String_Buffer. Larger sizes will minimze the
   -- number of calls to Append, with the trade off of a proportionatly
   -- increased size of Parser_State
   
package JSON.Parser_Machine is
   
   -- Basic operation: When intialized or Reset, the FSM is expecting any valid
   -- structure start ('{' or '['), after which time it issues a Push. This is
   -- the "Root" item of the blob.
   --
   -- Note that the parser will indicate Done once it encouteres the matching
   -- "Pop" condition, instead of indicating Pop.
   
   type Parser_FSM is limited private;
   
   type Operator_Indication is 
     (Ready,
      -- Machine is happy, but has nothing to output yet. Additional input
      -- should be fed into the machine until the indication changes.
      
      Output_Member,
      
      -- Machine has completed the parse of a new non-structural member value
      -- that must be extracted from the machine and appended to the current
      -- branch of the deserialized tree.
      --
      -- Legal JSON requires that all object members have a name. The machine
      -- has therefore parsed the members name, which can be extracted via
      -- Member_Name.
      --
      -- The Value can be extracted via Output_Kind and Output_Value
      --
      -- After obtaining the output, the operator should continue with the
      -- next Feed
      
      Output_Element,
      
      -- Machine has encountered the next element of the current array, which
      -- can be extracted via Value_Kind and Value. Array elements cannot have
      -- names, and so the operator must not invoke
      
      Push,
      
      -- Machine has encountered a new object or array which should be appended
      -- to the current tree, and forms a parent node for a new branch upon
      -- which subsequent Extract values should be appended.
      --
      -- The operator can query the type of the new child structure via
      -- Current_Structure_Kind. If the new structure was a member of an
      -- object, the name (which is required for valid JSON), can be extracted
      -- via Output_Name
      --
      -- After preparing the new branch, the operator should continue with the
      -- next Feed
      
      Pop,
      
      -- Machine has encountered the end of the object or array that is the
      -- current branch of the deserialized tree (but not the branch under
      -- root). Subsequent Extract values should be appended to the branch
      -- containing the parent node of the current (pre-Pop) branch.
      --
      -- After returning to the parent branch, the operator must invoked
      -- Acknowledge_Pop before continuing with the next Feed
      
      Done,
      
      -- The parser has completed deserializing an entire JSON object.
      
      Invalid);
   
      -- Invalid: The input was found to be invalid.
   
   procedure Setup_Buffers (Machine: in out Parser_FSM;
                            Config : in     JSON_String_Buffer_Config);
   
   -- This loads Config into Machine, and invokes Setup_Buffer on all internal
   -- State machine buffers. This procedure only needs to be called if
   -- Setup_Buffer needs to be called for declared JSON_String_Buffer objects
   --
   -- This does NOT need to be invoked after a Reset
   
   procedure Configure_Limits (Machine: in out Parser_FSM;
                               Limits : in     Codec_Limits);
   
   -- Configures Limits for the Machine. This configuration is NOT cleared on
   -- Reset (Limits cannot be cleared if set)
   
   function  Last_Indication (Machine: Parser_FSM) return Operator_Indication;
   
   -- Returns the "current" indication of the Machine, which was set after the
   -- last operation where Indication is an out parameter.
   
   type Text_Position is
      record
         Line  : Positive := 1;
         Column: Integer  := 0;
         
         Overflow: Boolean := False;
         -- If Line/Column reach their limits, Overflow is set to True,
         -- and no further attempt is made to increment the overflowed
         -- value (which will be at Positive/Natural'Last). This is to
         -- prevent potential large json strings that could "wrongly"
         -- (and inexplicably) invalidate the machine due to an
         -- exception
      end record;
   
   function  Last_Position (Machine: Parser_FSM) return Text_Position;
   -- Returns the total number of characters fed into the FSM since it was
   -- last initialized/reset. This will be the last inputted character of the
   -- serialized JSON text
   
   function  Current_Structure_Kind (Machine: Parser_FSM)
                                    return JSON_Structure_Kind;
   
   -- Returns that kind of structure the Machine belives it is currently
   -- "inside" of.
   
   procedure Feed (Machine   : in out Parser_FSM;
                   Input     : in     Wide_Wide_Character;
                   Indication:    out Operator_Indication) 
   with
     Pre  => Last_Indication (Machine) = Ready;
   
   -- Feed shall be called sequentially with the next character of the
   -- JSON object string after reacting to Indication as appropriate,
   -- until Indication is Pop, Output, or Push (which requires special operator
   -- action and acknowledment), or Halt (which indicates the parsing cannot
   -- continue).
   --
   -- If Indication is not Ready, the machine operator must act on
   -- the indication before executing the next Feed
   
   procedure Acknowledge_Pop (Machine   : in out Parser_FSM;
                             Popped_To : in     JSON_Structure_Kind;
                             Indication:    out Operator_Indication)
   with
     Pre  => Last_Indication (Machine) = Pop,
     Post => Indication = Ready
             and Current_Structure_Kind (Machine) = Popped_To;
   
   -- Must be called when the Indication is Pop. Popped_To shall be the
   -- kind of the level popped to. See the Pop Operator_Indication value above.
   --
   -- This implicitly causes the Indication to transition from Pop to Ready.
   
   procedure Acknowledge_Push (Machine   : in out Parser_FSM;
                               Indication:    out Operator_Indication) 
   with
     Pre  => Last_Indication (Machine) = Push,
     Post => Indication = Ready;
   
   -- Must be called when the Indication is Push. Push_Kind is the kind of the
   -- structure that should be pushed downed into before proceeding.
   --
   -- This implicitly causes the Indication to transition from Pop to Ready.

   
   procedure Output_Name (Machine: in out Parser_FSM;
                          Target :    out JSON_String_Buffer)
   with
     Pre => Last_Indication (Machine) in Output_Member | Push;
   -- Name is cleared during this operation (Export is invoked for the internal
   -- buffer -> Target).
   --
   -- Output_Name should only be invoked when Indication = Output_Member,
   -- or Indication = Push and the "previous" level (pre-push) is a
   -- JSON_Object. These must be checked by the operator, or subsequent name
   -- output may be corrupted
   
   function Output_Kind (Machine: Parser_FSM) return JSON_Value_Kind with
     Pre => Last_Indication (Machine) in Output_Member | Output_Element,
     Post => Output_Kind'Result not in JSON_Structure_Kind;
   
   procedure Output_Value (Machine: in out Parser_FSM;
                           Target :    out JSON_String_Buffer)
   with
     Pre => Output_Kind (Machine) = JSON_String;
   
   -- The output is cleared with this operation, but the output must still be
   -- Acknowledged
   
   function Output_Value (Machine: Parser_FSM) return Boolean with
     Pre => Output_Kind (Machine) = JSON_Boolean;
   
   function Output_Value (Machine: Parser_FSM) return JSON_Integer_Value with
     Pre => Output_Kind (Machine) = JSON_Integer;
   
   function Output_Value (Machine: Parser_FSM) return JSON_Float_Value with
     Pre => Output_Kind (Machine) = JSON_Float;
   
   procedure Acknowledge_Output (Machine   : in out Parser_FSM;
                                 Indication:    out Operator_Indication) 
   with
     Pre  => Last_Indication (Machine) in Output_Member | Output_Element,
     Post => Indication in Invalid | Ready | Pop;
   
   -- Must be called when the Indication is Output_Member or Output_Element.
   -- The output value can be obtained through the relevent operations before
   -- the output is acknowledged.

   function Invalid_Reason (Machine: Parser_FSM) return String with
     Pre => Last_Indication (Machine) = Invalid;
   
   -- Returns a string describing the invalidating condition that halted the
   -- machine
   
   procedure Emergency_Stop (Machine: in out Parser_FSM) with
     Post => Last_Indication (Machine) = Invalid;
   
   -- Causes the FSM to immediately become Invalid, but only if it was not
   -- already Invalid or Done. Invalid_Reason will return
   -- "Operator invoked Emergency Stop."
   
   procedure Reset (Machine: in out Parser_FSM) with
     Post => Last_Indication (Machine) = Ready;
   
   -- Resets the machine state only. This means that any configured limits
   -- remain. JSON_String_Buffers are NOT cleared, but if underlying
   -- storage needs to be reclaimed, it is safe to re-invoke Setup_Buffers
   -- after Reset. Such buffers will be cleared when first used after a
   -- Reset.
   --
   -- Setup_Buffers should be invoked after Reset.
   
private
   
   Integer_Max: constant := JSON_Integer_Value'Wide_Wide_Width;
   Float_Max  : constant := JSON_Float_Value'Wide_Wide_Width;
   
   type Machine_State is 
     -- All expected input is explicitly specified (including ignoring of
     -- Whitespace). Any input not specified by each state always causes:
     --
     -- State -> Halt,
     -- Indication -> Invalid

     
     (Start_Value,
      -- Machine is expecting the start of a value (any JSON_Value_Kind),
      -- after skipping any whitespace
      --
      -- '{':
      --    Level -> Level + 1
      --    Level_Kind -> JSON_Object
      --    In_Name -> True
      --    State -> Output
      --    Indication -> Push
      --
      -- '[':
      --   Level -> Level + 1
      --    Level_Kind -> JSON_Array
      --    In_Name -> False
      --    State -> Output
      --    Indication -> Push
      --
      -- (If Level = 0, then only '{' or '[' are accepted)
      --
      -- 'n':
      --   State -> In_Null (tail feed)
      --
      -- 't':
      --   State -> In_True (tail feed)
      --
      -- 'f':
      --   State -> In_False (tail feed)
      --
      -- '-' | '0-9':
      --   State -> In_Integer (tail feed)
      --
      -- '"':
      --   State -> In_String
      --   Indication -> Ready
      
      In_Null,
      -- Continue to fill Code_Buffer expecting to build towards a contents
      -- of exactly "null"
      --
      -- (Acceptible input):
      --    Indication -> Ready
      --
      -- ',' | '}' | ']':
      --    State -> Output
      --    Indication -> Output_Member/Element
      
      In_True,
      -- Continue to fill Code_Buffer expecting to build towards a contents
      -- of exactly "true"
      --
      -- (Acceptible input):
      --    Indication -> Ready
      --
      -- ',' | '}' | ']':
      --    State -> Output
      --    Indication -> Output_Member/Element
      
      In_False,
      -- Continue to fill Code_Buffer expecting to build towards a contents
      -- of exactly "false"
      --
      -- (Acceptible input):
      --    Indication -> Ready
      --
      -- ',' | '}' | ']':
      --    State -> Output
      --    Indication -> Output_Member/Element
      
      In_Integer,
      -- Expecting a JSON_Integer value. Note that JSON_Integer and JSON_Float
      -- literals are essentially compatible with Ada numeric literals, except
      -- that exponent is always 'E' (never 'e')
      --
      -- ['0']:
      --   (assert if IB_Level = IB'First then IB(IB'First) /= '0')
      --   Indication -> Ready
      --
      -- ['1' - '9']:
      --   Indication -> Ready
      --
      -- '-':
      --   (assert IB_Level = IB'First - 1)
      --   Indication -> Ready
      -- 
      -- '.':
      --   State -> In_Float
      --   Indication -> Ready
      --
      -- 'e' | 'E':
      --   State -> In_Integer_Exponent
      --   Indication -> Ready
      --
      -- ',' | ']' | '}'
      --   State -> Ouput
      --   Indication -> Output_Member/Element

      In_Float,
      -- Before transitioning to this state, In_Integer has already copied the
      -- buffer to the float buffer, and has appended the decimal point. This
      -- state simply collects the fractional value
      --
      -- ['0' - '9']:
      --   Indication -> Ready
      --
      -- 'e' | 'E':
      --   State -> In_Float_Exponent
      --   Indication -> Ready
      
      In_Integer_Exponent,
      -- This is a special case, but allowed by JSON. An example is "1E100".
      -- This is not a valid Ada literal, so the state machine will attempt to
      -- convert the exponent value into a corresponding number of zeros that 
      -- are appended to the literal. 
      --
      -- If '-' follows 'e', "e-" is replaced with ".0e-", and the state is
      -- set to be In_Float_Exponent
      -- 
      -- '-':
      --   State -> In_Float_Exponent
      --
      -- ',' | ']' | '}':
      --   State -> Output
      --   Indication -> Output_Member/Element
      --  
      
      In_Float_Exponent,
      -- Following 'e', we continue to copy in a valid exponent value
      --
      -- ['-' | '+' | '0' - '9']:
      --   Indication -> Ready
      --
      -- ',' | ']' | '}':
      --   State -> Output
      --   Indication -> Output_Member/Element
      
      Start_String,
      -- Expecting the start of a string. Whitespace is ignored
      --
      -- '"':
      --    State -> In_String
      --    Indication -> Ready
      
      In_String,
      -- We are inside of a string.
      --
      -- '"':
      --   State ->
      --     In_Name = True?
      --       In_Name -> False,
      --       State   -> After_Name
      --       Indication -> Ready
      --     In_Name = False?
      --       State -> Output
      --       Indication -> Output_(Member/Element)
      --
      -- '\':
      --   State -> In_String_Escape
      
      In_String_Escape,
      -- We are expecting a valid escape sequence, which
      -- will get built up into Code_Buffer iff Input is not one of the valid
      -- escaped characters.
      --
      -- [Any valy valid escaped character]:
      --   State -> In_String
      --
      -- 'u':
      --   CB_Level -> 0
      --   State -> In_Unicode_Escape
      --
      -- Indication -> Ready
      
      In_Unicode_Escape,
      -- We are expecting exactly a 4-digit hexadecimal string.
      --
      -- [hex digit]:
      --   CB_Level = < 4?
      --     State -> In_Unicode_Escape
      --   CB_Level = 4
      --     State -> In_String
      --
      -- Indication -> Ready
      

      After_Name,
      -- We are expecting a ':', ignoring Whitespace.
      --
      -- ':':
      --   In_Name -> False,
      --   State -> Start_Value,
      --   Indication -> Ready
      
      End_Value,
      -- Entered from Acknowledge_Output or Acknowledge_Pop
      --
      -- Depending on the Kind of the acknowledge condition, End_Value
      -- decides if it should transition into After_Value on next feed,
      -- or immediately with a tail feed
      --
      -- For strings, literals, and pops, After_Value requires a new
      -- feed.
      --
      -- For numbers After_Value takes a tail-feed.
      --
      -- This logic could have just been put in Acknowledge_Output or
      -- Acknowledge_Pop, but it seemd more proper to keeps as much of
      -- the state transition logic in one place as possible
      -- (Acknowledge_Push is a notable exception)
      
      After_Push,
      -- Entered from Acknowledge_Push, requiring a new feed.
      -- The purpose of this state is to accomodate empty structures.
      --
      -- Ignoring whitespace.
      --
      -- ']' | '}':
      --   State -> After_Value (tail feed)
      --
      -- (others):
      --   State -> Start_Value (tail feed)

      
      After_Value,
      -- We are expecting ',', ']', or '}', ignoring Whitepace.
      --
      -- ',':
      --   Level_Kind = JSON_Object?
      --     In_Name -> True
      --     State -> Start_String
      --
      --   Level_Kind = JSON_Array?
      --     In_Name -> False
      --     State -> Start_Value
      --
      --   Indication -> Ready
      --
      -- ']':
      --   (Assert Level_Kind = JSON_Array)
      --   State -> Output
      --   Indication -> Pop
      --
      -- '}':
      --   (Assert Level_Kind = JSON_Object)
      --   State -> Output
      --   Indication -> Pop
      
      Output,
      -- We are waiting for the user to obtain the last value, after which
      -- we can transition into After_Value directly
      
      Halt);
      -- Feed never enters on this state.
      -- Indication informs the reason for Halt, and should be either
      -- Done, or Invalid
   
   type Parser_FSM is limited
      record
         State     : Machine_State := Start_Value;
         Indication: Operator_Indication := Ready;
         Error     : access constant String := null;
         
         Input     : Wide_Wide_Character;
         Position  : Text_Position;
         -- Position indicates the position in the JSON stream of the most
         -- Recently inputed character. Position is incremented on entry to
         -- Feed, and can be used to identify offending input for invalid
         -- JSON streams
         
         Last_Was_CR: Boolean := False;
         -- Last_Was_CR is set to True if the previous Input was
         -- Carriage_Return, and State does not indicate that the machine is
         -- currently processing a string value. This is used to collapse a
         -- CR+LF sequence in whitespace, and increment Position.Line
         -- appropriately. This value is always set to False after processing 
         -- new Input, unless it is a CR character. This value is used and
         -- modified by Feed only, and the state machine itself does not regard
         -- it.
         --
         -- Input is always passed to the FSM which decides if the input is
         -- legal. Last_Was_CR is only used to decide when to (and when not to)
         -- increment the Position.Line value. Invalid sequences like a single
         -- CR or CR+LF will probably mangle the position counter, but if are
         -- valid JSON, will not invalidate the FSM.
         

         Level     : Natural := 0;
         Level_Kind: JSON_Structure_Kind := JSON_Object;
         -- Current tree level and kind
         
         Value_Kind   : JSON_Value_Kind;
         
         In_Name      : Boolean;
         Name_Output  : JSON_String_Buffer;
         
         Boolean_Output: Boolean;
         
         String_Output: JSON_String_Buffer;
         -- Contains accepted String value input
         
         Scratch_Buffer: JSON_String_Value (1 .. String_Chunk_Size);
         SB_Level: Natural;
         
         Code_Buffer: Wide_Wide_String (1 .. 6);
         CB_Level: Natural;
         -- Buffers input that is expected to be
         -- * A unicode excape sequence ("\u0000")
         -- * "true", "false" or "null"
         --
         -- It might be possible to use the Scratch_Buffer for the needs
         -- of Code_Buffer, but doing so would be needlessly (probably
         -- dangerously) complex.
         
         Integer_Buffer: Wide_Wide_String (1 .. Integer_Max);
         IB_Level: Natural;
         Integer_Output: JSON_Integer_Value;
         -- Buffers the input specific to an Integer value
         
         Float_Buffer: Wide_Wide_String (1 .. Float_Max);
         FB_Level: Natural;
         Float_Output: JSON_Float_Value;
         
         Limits        : Codec_Limits;
         Limits_Enabled: Boolean := False;
         -- Limits to be enforced by the machine
         
         String_Total: Natural := 0;
         -- The current number of characters within the string currently being
         -- deserialized (In_String et all), either a name or value. This is
         -- used to compare against the specified Limits, if enabled.
         
         Entity_Total: Natural := 0;
         -- The total number of output cycles (values parsed) since the
         -- machines was initialized. This is used to compare against the
         -- specified Limits, if enabled.
      end record;
   

end JSON.Parser_Machine;
