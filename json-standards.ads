------------------------------------------------------------------------------
--                                                                          --
--                         JSON Parser/Constructor                          --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2017-2021, ANNEXI-STRAYLINE Trans-Human Ltd.              --
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

-- This package contains all of the formal character and character sets
-- that are defined in the JSON standard (IETF RFD 8259/STD 90)

with Ada.Strings.Wide_Wide_Maps; use Ada.Strings.Wide_Wide_Maps;

package JSON.Standards is
   
   -- Whitespace 
   Space          : constant Wide_Wide_Character
     := Wide_Wide_Character'Val(16#0000_0020#);
   
   Horizontal_Tab : constant Wide_Wide_Character
     := Wide_Wide_Character'Val(16#0000_0009#);
   
   Line_Feed      : constant Wide_Wide_Character
     := Wide_Wide_Character'Val(16#0000_000A#);
   
   Carriage_Return: constant Wide_Wide_Character
     := Wide_Wide_Character'Val(16#0000_000D#);
   
   -- Whitespace set
   Whitespace: constant Wide_Wide_Character_Set := To_Set 
     (Wide_Wide_Character_Sequence'(1 => Space,
                                    2 => Horizontal_Tab,
                                    3 => Line_Feed,
                                    4 => Carriage_Return));
   
   
   -- Structural
   Begin_Array    : constant Wide_Wide_Character 
     := Wide_Wide_Character'Val(16#0000_005B#); -- '['
   End_Array      : constant Wide_Wide_Character 
     := Wide_Wide_Character'Val(16#0000_005D#); -- ']'
   
   Begin_Object   : constant Wide_Wide_Character 
     := Wide_Wide_Character'Val(16#0000_007B#); -- '{'
   
   End_Object     : constant Wide_Wide_Character
     := Wide_Wide_Character'Val(16#0000_007D#); -- '}'
   
   Name_Separator : constant Wide_Wide_Character
     := Wide_Wide_Character'Val(16#0000_003A#); -- ':'
   
   Value_Separator: constant Wide_Wide_Character
     := Wide_Wide_Character'Val(16#0000_002C#); -- ','
   

   Structural: constant Wide_Wide_Character_Set := To_Set
     (Wide_Wide_String'(1 => Begin_Array,
                        2 => End_Array,
                        3 => Begin_Object,
                        4 => End_Object,
                        5 => Name_Separator,
                        6 => Value_Separator));
   
   -- Nesting down set
   Nesting_Down : constant Wide_Wide_Character_Set
     := To_Set(Wide_Wide_String'(1 => Begin_Object, 2 => Begin_Array));
   
   Nesting_Up   : constant Wide_Wide_Character_Set
     := To_Set(Wide_Wide_String'(1 => End_Object, 2 => End_Array));
   
   
   -- Literals
   
   Literal_False_Start: constant Wide_Wide_Character 
     := Wide_Wide_Character'Val(16#0000_0066#);        -- 'f'
   
   Literal_False  : constant Wide_Wide_String 
     := (1 => Literal_False_Start,                     -- 'f'
         2 => Wide_Wide_Character'Val(16#0000_0061#),  -- 'a'
         3 => Wide_Wide_Character'Val(16#0000_006C#),  -- 'l'
         4 => Wide_Wide_Character'Val(16#0000_0073#),  -- 's'
         5 => Wide_Wide_Character'Val(16#0000_0065#)); -- 'e'
   
   Literal_True_Start: constant Wide_Wide_Character
     := Wide_Wide_Character'Val(16#0000_0074#);        -- 't'
   
   Literal_True   : constant Wide_Wide_String
     := (1 => Literal_True_Start,                      -- 't'
         2 => Wide_Wide_Character'Val(16#0000_0072#),  -- 'r'
         3 => Wide_Wide_Character'Val(16#0000_0075#),  -- 'u'
         4 => Wide_Wide_Character'Val(16#0000_0065#)); -- 'e'
   
   Literal_Null_Start: constant Wide_Wide_Character
     := Wide_Wide_Character'Val(16#0000_006E#);        -- 'n'
   
   Literal_Null   : constant Wide_Wide_String
     := (1 => Literal_Null_Start,                      -- 'n'
         2 => Wide_Wide_Character'Val(16#0000_0075#),  -- 'u'
         3 => Wide_Wide_Character'Val(16#0000_006C#),  -- 'l'
         4 => Wide_Wide_Character'Val(16#0000_006C#)); -- 'l'
   
   -- Numerics
   Decimal_Point: constant Wide_Wide_Character 
     := Wide_Wide_Character'Val(16#0000_002E#); -- '.'
   
   Minus_Sign   : constant Wide_Wide_Character
     := Wide_Wide_Character'Val(16#0000_002D#); -- '-'
   
   Plus_Sign    : constant Wide_Wide_Character
     := Wide_Wide_Character'Val(16#0000_002B#); -- '+'
   
   Exponent_Upper: constant Wide_Wide_Character 
     := Wide_Wide_Character'Val(16#0000_0045#); -- 'E'
   
   Exponent_Lower: constant Wide_Wide_Character
     := Wide_Wide_Character'Val(16#0000_0065#); -- 'e'
   
   Exponent_Set : constant Wide_Wide_Character_Set := To_Set
     (Wide_Wide_String'(1 => Exponent_Upper,
                        2 => Exponent_Lower));
   
   Integer_Terminator: constant Wide_Wide_Character_Set 
     := To_Set (Decimal_Point & To_Sequence (Exponent_Set));
   
   -- Values that follow the first set of contigious digits that make-up the
   -- (required) "int" portion of a json number value.
                    
   Num_Zero     : constant Wide_Wide_Character
     := Wide_Wide_Character'Val(16#0000_0030#); -- '0'
   
   Digit_1_9_Range  : constant Wide_Wide_Character_Range 
     := (Low  => Wide_Wide_Character'Val(16#0000_0031#),  -- '1'
         High => Wide_Wide_Character'Val(16#0000_0039#)); -- '9'
   
   Digit_1_9_Set: constant Wide_Wide_Character_Set := To_Set (Digit_1_9_Range);
   
   Digit_Range: constant Wide_Wide_Character_Range
     := (Low  => Num_Zero,
         High => Digit_1_9_Range.High);
   
   Digit_Set  : constant Wide_Wide_Character_Set := To_Set (Digit_Range);
   
   
   Numeric_Begin_Set: constant Wide_Wide_Character_Set
     := To_Set (Num_Zero & To_Sequence (Digit_1_9_Set) & Minus_Sign);
   -- Valid beginning to Numeric values
   
   Numeric_Continue_Set: constant Wide_Wide_Character_Set 
     := To_Set (To_Sequence (Digit_Set) & To_Sequence (Exponent_Set)
                  & Decimal_Point);
   
   
   Number_Termination_Set: constant Wide_Wide_Character_Set
     := To_Set (End_Object 
                  & End_Array 
                  & Value_Separator 
                  & To_Sequence (Whitespace));
   -- All valid character to indicate the end of a number value
   
   -- Strings
   Quotation_Mark    : constant Wide_Wide_Character 
     := Wide_Wide_Character'Val(16#0000_0022#);  -- '"'
   
   Reverse_Solidus   : constant Wide_Wide_Character
     := Wide_Wide_Character'Val(16#0000_005C#);  -- '\'
   
   Escape_Symbol     :          Wide_Wide_Character
     renames Reverse_Solidus;
   
   Universal_Code    : constant Wide_Wide_Character
     := Wide_Wide_Character'Val(16#0000_0075#);  -- 'u'
   
   -- Little problem here with maps - the sets from which the maps are derrived
   -- will be re-rodered by their character value, in ascending order! that
   -- means "abc" and "CBA" to a map will still translate from "abc" to "ABC",
   -- not "CBA". So we can't use maps to translate from escape codes to the
   -- actual stand-ins, instead we will use the indexting from a match in
   -- Escape_Code_List to the actual stand-in in the Escape_Actual_List
   
   -- Characters that may follow Escape_Symbol in a string
   Escape_Code_List  : constant Wide_Wide_String
     := (1 => Quotation_Mark,                           -- '"'
         2 => Reverse_Solidus,                          -- '\'
         3 => Wide_Wide_Character'Val(16#0000_002F#),   -- '/'
         4 => Wide_Wide_Character'Val(16#0000_0062#),   -- 'b'
         5 => Wide_Wide_Character'Val(16#0000_0066#),   -- 'f'
         6 => Wide_Wide_Character'Val(16#0000_006E#),   -- 'n'
         7 => Wide_Wide_Character'Val(16#0000_0072#),   -- 'r'
         8 => Wide_Wide_Character'Val(16#0000_0074#),   -- 't'
         9 => Universal_Code);                          -- 'uXXXX'
   
   Escape_Code_Set   : constant Wide_Wide_Character_Set
     := To_Set (Escape_Code_List);
   

   
   -- Characters that should replace the matching escape sequence
   Escape_Actual_List : constant Wide_Wide_String
     := (1 => Quotation_Mark,                           -- '"'
         2 => Reverse_Solidus,                          -- '\'
         3 => Wide_Wide_Character'Val(16#0000_002F#),   -- '/'
         4 => Wide_Wide_Character'Val(16#0000_0008#),   -- %backspace%
         5 => Wide_Wide_Character'Val(16#0000_000C#),   -- %FF%
         6 => Wide_Wide_Character'Val(16#0000_000A#),   -- %LF%
         7 => Wide_Wide_Character'Val(16#0000_000D#),   -- %CR%
         8 => Wide_Wide_Character'Val(16#0000_0009#),   -- %TAB%
         9 => Universal_Code);                          -- 'uXXXX'
   
   Escape_Actual_Set: constant Wide_Wide_Character_Set
     := To_Set (Escape_Actual_List);
   
   Escape_Serialize_Set: constant Wide_Wide_Character_Set
     := To_Set (Escape_Actual_List (1 .. 8));
   
   -- The list except for universal codes. This is used by the serializer to
   -- insert necessary escapes for string values. It doesn't need to re-encode
   -- uXXXX values, since it just encodes everything as utf_8. If the uXXXX
   -- encodes, for example, \u0008 (backpace), the serializer will just output
   -- "\/".
   
   Escape_Code_Map: constant Wide_Wide_Character_Mapping
     := To_Mapping (From => Escape_Code_List, To => Escape_Actual_List);
   
   Escape_Code_Reverse_Map: constant Wide_Wide_Character_Mapping
     := To_Mapping (From => Escape_Actual_List, To => Escape_Code_List);
   
   -- Valid values for \uXXXX hex values
   Escape_Hex_Set    : constant Wide_Wide_Character_Set :=
     To_Set(Wide_Wide_Character_Ranges'((Low  => '0',
                                         High => '9'),
                                        
                                        (Low  => 'a',
                                         High => 'f'),
                                        
                                        (Low  => 'A',
                                         High => 'F')));
   
   
   -- These characters MUST be escaped, and connot appear on their own
   -- inside of a string.
   Escape_Imparative: constant Wide_Wide_Character_Set := To_Set
     (Wide_Wide_Character_Ranges'
        ((Low  => Wide_Wide_Character'Val(16#0000_0000#),
          High => Wide_Wide_Character'Val(16#0000_001F#)),
         
         (Low  => Quotation_Mark,
          High => Quotation_Mark),
         
         (Low  => Reverse_Solidus,
          High => Reverse_Solidus)));
         
end JSON.Standards;

