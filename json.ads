------------------------------------------------------------------------------
--                                                                          --
--                         JSON Parser/Constructor                          --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2020, ANNEXI-STRAYLINE Trans-Human Ltd.                   --
--  All rights reserved.                                                    --
--                                                                          --
--  Original Contributors:                                                  --
--  * Richard Wai (ANNEXI-STRAYLINE)                                        --
--  * Ensi Martini (ANNEXI-STRAYLINE)                                       --
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

--with AURA.JSON;

with Ada.Streams;
with Ada.Containers;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.Wide_Wide_Bounded;

package JSON is
   
   subtype UTF_8_String is Ada.Strings.UTF_Encoding.UTF_8_String;
   
   type JSON_Value_Kind is 
     (JSON_Object, 
      JSON_Array, 
      JSON_String, 
      JSON_Integer, 
      JSON_Float, 
      JSON_Boolean,
      JSON_Null);
   
   subtype JSON_Structure_Kind is 
     JSON_Value_Kind range JSON_Object .. JSON_Array;
   
   -- JSON "primitive" types
   subtype JSON_String_Value  is Wide_Wide_String;
   subtype JSON_Integer_Value is Integer;
   subtype JSON_Float_Value   is Float;
   
   
   --------------------------------
   -- Codec Limits Configuration --
   --------------------------------
   
   -- The following configuration limit record type is used to configure the
   -- bounded properties of Bounded_JSON_Codecs, but may also be used to
   -- explicitly configure limits for Unbounded_JSON_Codecs.
   --
   -- Using Bounded_JSON_Codecs, or applying limits to Unbounded_JSON_Codecs
   -- provide excellent security (specifically DoS evasion) properties to
   -- externally sourced JSON streams.
   --
   -- While these properties are obligatory for Bounded_JSON_Codecs, it is
   -- often suboptimal to use Bounded_JSON_Codecs in environments where memory
   -- is plentiful, but security is important. In these environments, the
   -- the bounded codec can impose unacceptable memory overhead when the
   -- the inbound JSON stream is expected to be extremely variable.
   --
   -- Applying these limits to Unbounded_JSON_Codecs allows for the setting of
   -- appropriate limits without incurring a high baseline memory load that
   -- would be expected when applying a bounded codec to a highly dynamic
   -- object size.
   
   type Codec_Limits is
      record
         String_Limit: Positive;
         
         -- Configures a maximum length for JSON_String values
         -- (in Wide_Wide_Characters) Deserialized into the Codec. The default
         -- is unlimited. If a limit is set, any string that exceeds this limit
         -- will cause the parser state machine to halt. Invalid will then
         -- return True, and the condition will be expressed via Error_Message
         
         Entity_Limit: Positive;
         
         -- Configures the maximum number of values (primitive or structural)
         -- that may be Deserialized into the Codec. This value, multiplied
         -- by String_Limit, multiplied by the size of a Wide_Wide_Character
         -- gives a wost-case memory consumption estimate before the limit is
         -- triggered
         
         Depth_Limit: Positive;
         
         -- Configures the maximum depth of the Deserialized JSON tree. A depth
         -- of 1 would not allow for any objects or arrays in the root object.
         --
         -- This limit is potentially extremely effective against attacks,
         -- since maximum depths are often known, or can be fixed, as a
         -- property of an application's design.
         --
         -- If no known depth limit is applicable, this value can be set to
         -- equal Entity_Limit, which is then effectively "unlimited"
         
      end record;
   
end JSON;
