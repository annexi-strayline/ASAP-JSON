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

-- This is the unbounded codec
with Ada.Iterator_Interfaces;

private with Interfaces;
private with Ada.Finalization;
private with System.Storage_Pools.Subpools;

private with JSON.Fast_Slab_Allocators;
private with JSON.Parser_Machine;

package JSON.Unbounded_Codecs is
   
   ----------------
   -- JSON_Value --
   ----------------
   
   -- JSON_Values represent active members of an object, or elements of an
   -- array, within a Codec
   
   type JSON_Value(<>) is tagged limited private;
   
   function Kind        (Value: JSON_Value) return JSON_Value_Kind;
   function Parent_Kind (Value: JSON_Value) return JSON_Structure_Kind;
   
   -- Returns the structure kind of the Parent structure of Value
   -- (JSON_Object or JSON_Array).
   --
   -- Invoking Parent_Kind on the Root object raises Contraint_Error
   
   function Name (Value: JSON_Value) return JSON_String_Value with
     Pre'Class => Value.Parent_Kind = JSON_Object;
   
   function UTF8_Name  (Value: JSON_Value) return UTF_8_String with
     Pre'Class => Value.Parent_Kind = JSON_Object;
   
   -- Returns the name of Value, if it is an Object Member.
   -- Constraint_Error is raised if Value is not an Object Member.
   
   function Index (Value: JSON_Value) return Natural;
   
   -- Returns a zero-based index of Value in its structure. This value can be
   -- produced for Object Members or Array Elements, but this Index can only
   -- be used to index JSON_Array JSON_Structures. In both cases, Index is
   -- in the order of deserialization or appendage of the value.
   
   function Value      (Value: JSON_Value) return Boolean;
   function Value      (Value: JSON_Value) return JSON_Integer_Value;
   function Value      (Value: JSON_Value) return JSON_Float_Value;
   function Value      (Value: JSON_Value) return JSON_String_Value;
   function UTF8_Value (Value: JSON_Value) return UTF_8_String;
   
   -- If the Kind of Value is incompatible with the expected return type,
   -- a Discriminent_Check will fail at runtime
   
   -- Note that for structural Kinds (JSON_Object and JSON_Array), the
   -- corresponding JSON_Value can be used as an index value for the
   -- Unbounded_JSON_Codec 
   
   -- Value editing operations --
   
   procedure Value (Target: in out JSON_Value;
                    Value : in     Boolean;
                    Mutate: in     Boolean := False)
   with Pre'Class =>
     (if not Mutate then Target.Kind in JSON_Boolean | JSON_Null);
   
   procedure Value (Target: in out JSON_Value; 
                    Value : in     JSON_Integer_Value;
                    Mutate: in     Boolean := False)
   with Pre'Class => 
     (if not Mutate then Target.Kind in JSON_Integer | JSON_Null);
   
   procedure Value (Target: in out JSON_Value;
                    Value : in     JSON_Float_Value;
                    Mutate: in     Boolean := False) 
   with Pre'Class => 
     (if not Mutate then Target.Kind in JSON_Float | JSON_Null);
   
   procedure Value (Target: in out JSON_Value;
                    Value : in     JSON_String_Value;
                    Mutate: in     Boolean := False)
   with Pre'Class =>
     (if not Mutate then Target.Kind in JSON_String | JSON_Null);
   
   -- Sets the value of Target. If Mutate is False, Constraint_Error is
   -- raised if the Kind of Target does not fit the type of Value.
   --
   -- Otherwise, is Mutuate is True, the Target is mutated into the
   -- Kind associated with the type of Value.
   --
   -- Null values can always be set to any type, regardless of the
   -- value of Mutate.
   --
   -- Note: Mutating a JSON_String value to a non-string value
   -- of any kind causes the dynamically allocated value to be lost,
   -- and the underlying storage is not reclaimed until the codec is
   -- finalized.
   --
   -- It is certainly pathelogical to be mutating any Values frequently
   -- or many times, but in the case of JSON_String values specifically,
   -- this could appear to cause a memory leak until the Codec is
   -- finalized. This behaviour is a result of the performance and low
   -- memory fragmentation design, and the sane assumption that these
   -- sorts of mutations will not happen more than once per JSON_String
   -- value.

   
   procedure Nullify (Target: in out JSON_Value); 
   
   -- Sets Target to a 'null' value.
   
   type JSON_Value_Reference (Ref: not null access JSON_Value) is 
     null record with Implicit_Dereference => Ref;
   
   type JSON_Value_Constant_Reference 
     (Ref: not null access constant JSON_Value) is 
     null record with Implicit_Dereference => Ref;

   
   --------------------
   -- JSON_Structure --
   --------------------
   
   type JSON_Structure is tagged limited private with
     Constant_Indexing => Constant_Reference,
     Iterator_Element  => JSON_Value,
     Default_Iterator  => Iterate;
   
   -- Represents the root of a JSON_Object or JSON_Array, and is referenced via
   -- a corresponding JSON_Object or JSON_Array JSON_Value
   
   function Kind (Structure: JSON_Structure) return JSON_Structure_Kind;
   
   function Length (Structure: JSON_Structure) return Natural;
   
   -- Returns the number of Elements or Members that are direct children of
   -- Structure
   
   -- Array indexing --
   
   subtype  JSON_Array_Index is Natural;
   
   -- Using the Javascript convention for zero-based indexing
   
   function Constant_Reference (Structure: JSON_Structure; 
                                Index    : JSON_Array_Index)
                               return JSON_Value_Constant_Reference with
     Pre'Class  => Structure.Kind = JSON_Array and Index < Structure.Length;
   
   -- Index is a Javascript-style, zero-based array index. Note that,
   -- internally, the Codec represents the entire JSON object as a tree, and
   -- therefore Arrays are internally linked-lists. In order to provide the
   -- expected runtime qualities, the first access of a JSON_Array object via
   -- this index-based (Constant_)Reference operation causes the array to be
   -- "vectorized". Vectorization is done "lazily" since it encurs a
   -- potentially high memory cost.
   --
   -- If the Index is out of range (> Length), Contraint_Error is raised.
   
   -- Object indexing --
   
   function Has_Member (Structure: JSON_Structure; Name: JSON_String_Value)
                       return Boolean with
     Pre'Class => Structure.Kind = JSON_Object;
   
   -- Returns True iff Structure contains a member named Name.
   -- A Discrimint_Check fails if Structure is not a JSON_Object.
   
   function Constant_Reference (Structure: JSON_Structure;
                                Name     : JSON_String_Value)
                               return JSON_Value_Constant_Reference with
     Pre'Class => Structure.Has_Member (Name);
   
   -- Name must be a direct name for a member in the the first level
   -- of Structure (which must also be an object). Paths cannot be used on
   -- JSON_Structure objects. For path indexing, use the Unbounded_JSON_Codec
   -- Lookup indexing operations
   --
   -- If Has_Member (Name) = False, Constraint_Error is raised.
   
   
   -- Structure Iteration --
   
   type JSON_Structure_Cursor is limited private;
   
   function Has_Value (Position: JSON_Structure_Cursor) return Boolean;
   
   function Constant_Reference (Structure: JSON_Structure;
                                Position : JSON_Structure_Cursor)
                               return JSON_Value_Constant_Reference with
     Pre'Class => Has_Value (Position);
   
   -- Constraint_Error is raised if Position is not a valid cursor, either
   -- because it is null, or because it belongs to a different codec.
   
   package JSON_Structure_Iterators is new Ada.Iterator_Interfaces
     (Cursor      => JSON_Structure_Cursor,
      Has_Element => Has_Value);
   
   function Iterate (Structure: JSON_Structure) return
     JSON_Structure_Iterators.Reversible_Iterator'Class;
   
   ----------------------------
   -- JSON_Mutable_Structure --
   ----------------------------
   
   type JSON_Mutable_Structure is new JSON_Structure with private with
     Variable_Indexing => Reference;
   
   -- JSON_Mutable_Structures are obtained via the Unbounded_JSON_Codec Delve
   -- operation, and can be used to obtain variable references for all values
   -- within the underlying structure value. 
   
   not overriding
   function Reference (Structure: in out JSON_Mutable_Structure;
                       Index    : in     JSON_Array_Index)
                      return JSON_Value_Reference with
     Pre'Class  => Structure.Kind = JSON_Array and Index < Structure.Length;
   
   not overriding
   function Reference (Structure: in out JSON_Mutable_Structure;
                       Name     : in     JSON_String_Value)
                      return JSON_Value_Reference with
     Pre'Class => Structure.Has_Member (Name);
   
   not overriding
   function Reference (Structure: in out JSON_Mutable_Structure;
                       Position : in     JSON_Structure_Cursor)
                      return JSON_Value_Reference with
     Pre'Class => Has_Value (Position);
   
   -- Structure Building --
   
   not overriding
   function Append_Null_Member (Structure: in out JSON_Mutable_Structure;
                                Name     : in     JSON_String_Value)
                               return JSON_Value_Reference
   with Pre'Class => Structure.Kind = JSON_Object and Name'Length > 0,
     Post'Class => Append_Null_Member'Result.Ref.Kind = JSON_Null;
   
   not overriding
   function Append_Null_Element (Structure: in out JSON_Mutable_Structure)
                                return JSON_Value_Reference
   with
     Pre'Class => Structure.Kind = JSON_Array,
     Post'Class => Append_Null_Element'Result.Ref.Kind = JSON_Null;
   
   -- Appends a new null value member or element to the structure, and returns
   -- a reference to the new value. The new value can then be mutated normally
   
   not overriding
   function Append_Structural_Member 
     (Structure: in out JSON_Mutable_Structure;
      Name     : in     JSON_String_Value;
      Kind     : in     JSON_Structure_Kind)
     return JSON_Value_Reference
   with Pre'Class  => Structure.Kind = JSON_Object;
                                    
   
   not overriding
   function Append_Structural_Element 
     (Structure: in out JSON_Mutable_Structure;
      Kind     : in     JSON_Structure_Kind)
     return JSON_Value_Reference
   with Pre'Class  => Structure.Kind = JSON_Array;
   
   -- Appends a new structure of Kind to Strucure, and returns a reference.
   -- The returned reference can be used to obtain a JSON_(Mutable_)Structure
   -- from the Codec.
   
   --------------------------
   -- Unbounded_JSON_Codec --
   --------------------------
   
   type Unbounded_JSON_Codec(<>) is tagged limited private with
     Variable_Indexing => Lookup,
     Constant_Indexing => Constant_Lookup;
   
   -- Note that the indexing aspects of the codec are intended to facilitate
   -- direct path-based lookup. Iteration over the entire Tree can easily be
   -- acheived by iterating over the value of Root
   
   -- Tree-wide lookup --
   
   function Path_Exists (Codec: Unbounded_JSON_Codec;
                         Path : in JSON_String_Value)
                        return Boolean;
   
   function Lookup (Codec: aliased in out Unbounded_JSON_Codec;
                    Path :         in     JSON_String_Value)
                   return JSON_Value_Reference
   with Pre'Class => Codec.Path_Exists (Path);
   
   function Constant_Lookup (Codec: aliased Unbounded_JSON_Codec;
                             Path :         JSON_String_Value)
                            return JSON_Value_Constant_Reference
   with Pre'Class => Codec.Path_Exists (Path);
   
   -- Path is a dot-notation, javascript-style path to a member within some
   -- hierarchy of JSON Objects in the Codec. Note that it is the
   -- responsibility of the user to ensure name uniqueness, and to avoid using
   -- '.' in names of members. Doing so may potentially cause unexpected 
   -- double-registration errors when deserializing or encoding.
   --
   -- If Path is not valid (no value exists), a Constraint_Error will be raised
   -- due to the null exclusion of the returned reference type.
   --
   -- Paths CAN include array indexing ("[x]"). These follow javascript
   -- conventions (zero-based indexing)
   --
   -- A hash table is used to accelerate path indexing, making this approach
   -- extremely efficient for extracting expected items.
   --
   -- Examples of Lookup indexing:
   --
   -- declare
   --   My_Codec: Unbounded_JSON_Codec := Unbounded_JSON_Codec'Input (...);
   -- begin
   --   if My_Codec("Country.Canada.Toronto.Temperatures[0]").Value > 20.0 then
   --   ..
   --   if My_Codec("My_3D_Array[12][1][0]").Kind = JSON_Null then
   --
   --   My_Codec("Sender_ID").Value (My_ID);  -- Set the ID
   
   function Root (Codec: aliased in out Unbounded_JSON_Codec)
                 return JSON_Mutable_Structure'Class;
   
   function Constant_Root (Codec: aliased Unbounded_JSON_Codec)
                          return JSON_Structure'Class;

   -- Returns the root structure the represents the entire deserialized
   -- JSON object
   --
   -- If the tree is not Ready, Program_Error is raised.
   
   function Delve (Codec    : aliased in out Unbounded_JSON_Codec;
                   Structure: aliased in out JSON_Value'Class)
                   return JSON_Mutable_Structure'Class with
     Pre'Class => Structure.Kind in JSON_Structure_Kind;
   
   function Constant_Delve (Codec    : aliased Unbounded_JSON_Codec;
                            Structure: aliased JSON_Value'Class)
                           return JSON_Structure'Class with
     Pre'Class => Structure.Kind in JSON_Structure_Kind;
   
   -- Delve effectivly converts a Structure JSON_Value_Reference into a
   -- JSON_Structure object that can then be inspected.
   --
   -- Structure shall be a reference obtained from Codec, otherwise
   -- Program_Error is raised.
   --
   -- Failing the precondition can result in a null check or a discriminent
   -- check failure.
   
   -- Construction --
   
   function Construction_Codec (Format    : JSON_Structure_Kind := JSON_Object;
                                Write_Only: Boolean             := True)
                               return Unbounded_JSON_Codec;
   
   -- Initializes a new Codec that is intended for object construction and
   -- later serialization.
   --
   -- Format sets the Kind of the Root structure.
   --
   -- If Write_Only is True, paths are not registered, and path-based indexing
   -- is not available. This improves performance and reduces memory usage for
   -- constructions.
   
   
   -- Parsing and Generation --
   
   function Valid (Codec: Unbounded_JSON_Codec) return Boolean;
   
   -- Returns False if the parser has discovered an invalid condition,
   -- including for reasons not necessarily related to the parser (such as an
   -- End_Error)
   
   function Invalid_Because_End_Error (Codec: Unbounded_JSON_Codec)
                                      return Boolean
   with
     Pre'Class => not Codec.Valid;
   
   -- Returns True iff the inValid condition was specifically caused by the
   -- raising of Ada.IO_Exceptions.End_Error, indicating the end of a stream.
   --
   -- The will only be useful when doing stream deserializations
   
   function Error_Message (Codec: Unbounded_JSON_Codec) return String with
     Pre'Class => not Codec.Valid;
   
   -- Returns a messages explaining why the parser considers the input to
   -- be invalid, which is prepended to a "Line:Column:" indication to
   -- give an indication of where the error occured.
   -- 
   -- If the tree is Valid, Constraint_Error is raised
   
   function  Serialized_Length (Codec: in Unbounded_JSON_Codec) 
                               return Natural;
   
   -- Returns the number of ** UTF_8 ** characters required to serialize
   -- (generate) the JSON string representation of the Root object. Note
   -- that this value is generated by executing a Serialize dry-run and
   -- metering the output.
   
   procedure Serialize (Codec : in    Unbounded_JSON_Codec;
                        Output:   out UTF_8_String) with
     Pre'Class => Output'Length = Codec.Serialized_Length;
   
   -- Generates a UTF-8 encoded JSON string serialized from the tree.
   -- If Output is not large enough, Constraint_Error is raised.
   -- If Output is too large, the remaining space is filled with
   -- whitespace.
   
   function  Serialize (Codec: in Unbounded_JSON_Codec) 
                       return UTF_8_String with
     Post'Class => Serialize'Result'Length = Codec.Serialized_Length;
   
   -- Generates a UTF-8 encoded JSON string serialized from the tree. This
   -- process is two-pass, since Serialized_Length is invoked to size the
   -- returned string.
   
   procedure Serialize 
     (Output_Stream: not null access Ada.Streams.Root_Stream_Type'Class;
      Codec        : in Unbounded_JSON_Codec);
   
   -- Generates a UTF-8 encoded JSON string serialized from the tree, and
   -- writes it to Output_Stream. Note that this is the most efficient
   -- approach, as it is always single-pass.
   
   -- -- NOTE --
   -- Deserialization of a Codec never leaves the Codec in an inconsistent
   -- state, even if the input is invalid. Serializing a Codec where Valid is
   -- False will always produce valid JSON, however the contents of the Codec
   -- will likely be incompelete.

   function  Deserialize (Input: UTF_8_String) return Unbounded_JSON_Codec;
   
   -- Beware of extremely large Inputs. Input is converted into a
   -- Wide_Wide_String, and if that fails (such as due to a Storage_Error),
   -- the Codec will be invalidated with an "Emergency Stop", which may not
   -- be a very helpful message without reading this comment.
   
   function  Deserialize
     (Source: not null access Ada.Streams.Root_Stream_Type'Class)
     return Unbounded_JSON_Codec;
   
   function  Deserialize
     (Source: not null access Ada.Streams.Root_Stream_Type'Class;
      Limits: Codec_Limits)
     return Unbounded_JSON_Codec;
   
   function Time_Bounded_Deserialize
     (Source: not null access Ada.Streams.Root_Stream_Type'Class;
      Budget: in Duration)
     return Unbounded_JSON_Codec;
   
   function Time_Bounded_Deserialize
     (Source: not null access Ada.Streams.Root_Stream_Type'Class;
      Limits: Codec_Limits;
      Budget: in Duration)
     return Unbounded_JSON_Codec;
   
   -- Given a stream that shall be a UTF-8 text stream encoding a JSON
   -- object, Deserialize parses the stream until a complete JSON object
   -- has been deserialized.
   --
   -- Deserialize suppresses any exceptions experienced during the 
   -- deserialization process. If any such exception occurs, the Codec
   -- returned, but is marked inValid.
   --
   -- The user should check Valid immediately after initializing a Codec
   -- with Deserialize, or using 'Input.
   --
   -- In trule exceptional cases, if the Codec experiences exceptions during
   -- initialization of the Unbounded_JSON_Codec object itself (likely
   -- a Storage_Error), that exception is propegated.
   --
   -- This approach to error handling is intended for defensive server
   -- environments where any erronious input is discarded as quickly as
   -- possible, and the offending client disconnected.
   --
   -- For more complex error handling, particularily is client feedback
   -- is needed, the user should implement their own processes via the
   -- FSM_Push facilities
   --
   -- The Time_Bounded_Deserialize permutations implement integrated 
   -- "slow loris" attack protection by aborting the decode if the total
   -- time spent decoding exceeds Budget before the decode completes.
   --
   -- The elapsed time is checked after each unicode character is decoded
   -- from the Source stream. 
   --
   -- A "slow loris" attack is when a client sends single character
   -- transmissions at a rate that is (optimally) just below the connection's
   -- receive timeout, thus allowing for a connection to remain active for
   -- an extreme amount of time. These permuations thus protect against this
   -- buy defining a hard deadline by which the decode operation must complete



   
   procedure Disallowed_Read
     (Stream: not null access Ada.Streams.Root_Stream_Type'Class;
      Item  : out Unbounded_JSON_Codec)
   with No_Return;
   
   -- 'Read is not supported, since Codecs are stateful, and thus would require
   -- an in-out mode parameter. Invoking 'Read thus raised Program_Error.
   
   for Unbounded_JSON_Codec'Write  use Serialize;
   for Unbounded_JSON_Codec'Output use Serialize;
   for Unbounded_JSON_Codec'Read   use Disallowed_Read;
   for Unbounded_JSON_Codec'Input  use Deserialize;
   
   ------------------------------
   -- Unbounded_FSM_JSON_Codec --
   ------------------------------
   
   type Unbounded_FSM_JSON_Codec is limited new Unbounded_JSON_Codec with
     private;
   
   -- Default initialized Unbounded_FSM_JSON_Codecs do not have valid
   -- Roots. It must be initialized via repeated calls to FSM_Push as
   -- described below.
   
   not overriding
   function Input_Limited_FSM (Limits: Codec_Limits)
                              return Unbounded_FSM_JSON_Codec;
   
   -- Used to initalize a codec with input limits for the parser.
   
   not overriding
   procedure FSM_Push (Codec         : in out Unbounded_FSM_JSON_Codec;
                       Next_Character: in     Wide_Wide_Character;
                       Halt          :    out Boolean);
   
   not overriding
   procedure FSM_Push (Codec          : in out Unbounded_FSM_JSON_Codec;
                       Next_Characters: in     Wide_Wide_String;
                       Halt           :    out Boolean);
   
   -- Drives the Finite State Machine parser directly with a single character
   -- or sequence of characters.
   --
   -- FSM_Push should be invoked with new input until Halt is set to True.
   --
   -- The FSM will Halt for one of two reasons: Either it finished parsing
   -- a complete JSON object, or it encountered invalid JSON.
   -- 
   -- If Valid is True and Halt is also True, the deserialization has completed
   -- successfully.
   -- 
   -- Calling FSM_Push after Halt was signaled will have no effect on the
   -- Codec, but will cause Error_Message to report encode an incorrect
   -- position.
   --
   -- FSM_Push is not expected to raise exceptions related to the input.
   --
   -- These operations are exposed to allow for more direct control of
   -- Deserialization streaming, particularily to prevent "slow loris" attacks.
   --
   -- ** FSM_Push must only be called from a Codec initailized from **
   -- ** Manual_Parser, otherwise Program_Error is raised.          **
   --
   -- Advanced Users:
   -- ---------------
   -- The Codec remains "consistent" during parsing, even in the event of an
   -- inValid state. This means that the Codec can be queried and even modified
   -- before parsing is completed, at the possible risk of exceptions. However
   -- there is no danger of errnoneous execution other un-safe effects.
   --
   -- This could be potentialy useful for doing very early validation of
   -- JSON blobs
   
   not overriding
   function Root_Initialized (Codec: Unbounded_FSM_JSON_Codec) return Boolean;
   
   -- Returns True if the Root structure exists. This is useful for proactively
   -- avoiding exceptions by querying Root before the FSM Halts. (See note for
   -- "Advanced Users" above)
   
   
   for Unbounded_FSM_JSON_Codec'Write  use Serialize;
   for Unbounded_FSM_JSON_Codec'Output use Serialize;
   for Unbounded_FSM_JSON_Codec'Read   use Disallowed_Read;
   for Unbounded_FSM_JSON_Codec'Input  use Deserialize;
   
   -- Note that all of the deserialization operations work just as for regular
   -- Unbounded_JSON_Codecs, but these operations drive the same internal FSM,
   -- and will render FSM_Push useless, since the FSM state will Halted
   -- immediately after deserialization in all cases, and new input will be
   -- ignored.
   
   
   
private
   
   --------------------------
   -- Fast_Slab_Allocators --
   --------------------------   
   
   -- A custom subpool-capable storage pool optimized to maximize performance,
   -- and especially to minimize heap fragmentation for very high-throughput,
   -- long-uptime server applications.
   --
   -- Each Unbounded_JSON_Codec allocates all items from its own Subpool, and
   -- all deallocation occurs at finalization. Additionally, heap allocations
   -- are done in constant-size (should be page-sized) allocations (slabs).
   -- The Slab size can be set via the AURA configuration package with the
   -- configuration value Unbounded_Codec_Slab_Size
   
   package Fast_Slab_Allocators renames JSON.Fast_Slab_Allocators;
   
   -- GNAT Bug workaround. See the Fast_Slab_Allocators spec for details.
   
   Slab_Pool: Fast_Slab_Allocators.Slab_Pool_Designator
     renames Fast_Slab_Allocators.Global_Slab_Pool;
   
   subtype Subpool_Handle is System.Storage_Pools.Subpools.Subpool_Handle;
   use type Subpool_Handle;
   
   ------------------
   -- Slab_Strings --
   ------------------
   
   -- Slab_String is a slab allocator-friendly (low fragmentation) unbounded 
   -- non-contigious string type
   
   package Slab_Strings is
      
      type Slab_String is private;
      type Slab_String_Access is access Slab_String with
        Storage_Pool => Slab_Pool;
      
      -- Slab_String is kind of a super-private type. If it was general exposed
      -- to the outside world, it would be limited. In this case, it is
      -- non-limited to permit the mutability of JSON_Value objectes
      --
      -- Copying a Slab_String will not cause anything crazy to happen, except
      -- that it might eventually result in a corrupted string, since two
      -- "separate" strings would ultimately start overwriting eachother,
      -- however it would still remain technically memory safe, since all
      -- slab allocations are deallocated on dinalization of the entire Codec
      --
      -- Never make copies of Slab_String objects. Either use Transfer, or
      -- or ensure the "original" is never used after the copy.
      
      procedure Setup (Target : in out Slab_String;
                       Subpool: in     not null Subpool_Handle);
      
      -- Must be invoked prior to invoking any of the following operations.
      
      function "=" (Left, Right: Slab_String) return Boolean;
      
      procedure Append (Target: in out Slab_String;
                        Source: in     JSON_String_Value);
      
      procedure Clear (Target: in out Slab_String);
      
      procedure Transfer (From: in out Slab_String;
                          To  :    out Slab_String);
      
      -- After From is Transfered to To, From is Cleared. To does not need
      -- to be Set-up first. To will be associated with the same Subpool as
      -- From.
      
      function Length (S: Slab_String) return Natural;
      
      procedure To_JSON_String (Source: in     Slab_String;
                                Target:    out JSON_String_Value)
      with Pre => Target'Length = Length (Source);
      
      function To_JSON_String (Source: in Slab_String) 
                              return JSON_String_Value;
      

      
      procedure Write_Stream 
        (Stream: not null access Ada.Streams.Root_Stream_Type'Class;
         Item  : in Slab_String);
      
      for Slab_String'Write use Write_Stream;
      
      -- Stream Writing is intended for hashing
      
      Chunk_Size: constant := 64;
      -- The static Wide_Wide_Character'Length of each Subpool allocated
      -- "chunk" (segment). The string will grow by this size on demand
      
   private
   
      subtype Slab_String_Chunk_Index is Natural range 0 .. Chunk_Size;
      
      type Slab_String_Chunk;
      type Slab_String_Chunk_Access is access Slab_String_Chunk with
        Storage_Pool => Slab_Pool;
      
      type Slab_String is
         record
            Subpool      : Subpool_Handle           := null;
            
            Chain_First  : Slab_String_Chunk_Access := null;
            Chain_Last   : Slab_String_Chunk_Access := null;
            
            Current_Chunk: Slab_String_Chunk_Access := null;
            Current_Last : Slab_String_Chunk_Index  := 0;
            -- Current_Last is the current index of the last character
            -- of Current_Chunk, which is the last Chunk of the current
            -- string. If Current_Chunk is not Chain_First, then it
            -- is taken that all chunks before Current_Chunk are "full"
         end record;
      
   end Slab_Strings;
   
   --------------------
   -- JSON_Structure --
   --------------------
   
   type Node is access all JSON_Value with Storage_Pool => Slab_Pool;
   -- We make Node a general access type so that we can assign values of
   -- JSON_Value_Reference access discriminents to Node. This will always
   -- be safe since the user is not able to create references to JSON_Values
   -- that have no been generated via Node allocations, since JSON_Value has
   -- unknown descriminents.
   
   type JSON_Structure_Cursor is
      record
         Target: Node := null;
      end record;
   
   -- This is a general access type to allow us to assign reference values
   -- (access descriminents pointing at a JSON_Value) to values of this type.
   --
   -- We know that all JSON_Values coming this way ultimately arrived via
   -- allocators, since the user as no way of directly supplying
   

   
   type Constant_Codec_Access is not null access constant Unbounded_JSON_Codec 
   with Storage_Size => 0;
   
   type Mutable_Codec_Access is not null access all Unbounded_JSON_Codec 
   with Storage_Size => 0;
   
   type JSON_Structure is tagged limited
      record
         Structure_Root: not null Node;
         -- Always made to point to a JSON_Value with Kind of JSON_Object or
         -- JSON_Array. All children of the structure are children of this
         -- node.
         
         Codec_Constant: Constant_Codec_Access;
      end record;
   
   type JSON_Mutable_Structure is new JSON_Structure with 
      record
         Codec_Mutable: Mutable_Codec_Access;
      end record;
   
   --------------------
   -- Node_Hash_Maps --
   --------------------
   
   package Node_Hash_Maps is
      
      type Node_Hash_Map is private;
      
      -- Node_Hash_Map is designed to be very compact on default initialization
      -- so that it can be included in all structural nodes, only taking up
      -- space as needed
      --
      -- Similar to Slab_Strings, Node_Hash_Map would be limited if it was not
      -- such a private type. Copying Node_Hash_Maps will not really cause any
      -- issue, except that the Performance values will not be accurate.
      --
      -- Node_Hash_Maps are made non-limited to preserve internal mutability of
      -- the JSON_Value objects (which are publically limited)
      
      procedure Setup (Map    : in out Node_Hash_Map;
                       Subpool: in     not null Subpool_Handle);
      
      -- Configures a hash map with an appropriate Subpool for allocations
      
      
      procedure Register (Path_Map  : in out Node_Hash_Map; 
                          Registrant: in     not null Node);
      
      -- Use Name or Index component of Registrant (depending on the Parent),
      -- and adds that registration to the appropriate map of the Parent,
      -- along with the computed full-path registration to the Path_Map

      
      -- Both Register operations also register the full path with the Path_Map
      -- Raises Constraint_Error if Path/Name/Index has already been registered
      
      
      function  Lookup_By_Path (Path_Map: Node_Hash_Map;
                                Path    : JSON_String_Value)
                               return Node;
      
      function  Lookup_By_Name (Name_Map: Node_Hash_Map; 
                                Name    : JSON_String_Value)
                               return Node;
      
      function  Lookup_By_Index (Index_Map: Node_Hash_Map;
                                 Index    : Natural)
                                return Node;
      
      -- If the lookup fails, a null value is returned
      
      -- Monitoring services
      
      type Match_Level is range 1 .. 4;
      
      Performance_Monitoring: constant Boolean := True;
      
      type Level_Registrations is array (Match_Level) of Natural;
      -- Number of registrations per match level
      
      type Performance_Counters is
         record
            Primary_Registrations : Level_Registrations := (others => 0);
            Overflow_Registrations: Level_Registrations := (others => 0);
            
            -- Primary_Registrations are registrations made to the "head table"
            -- Secondary_Registrations are registrations made to additional
            -- tables added on demand.
            --
            -- Each tables is indexed with two different alternating hash-based
            -- strategies. These strategies are repeated over every even and
            -- odd table in the table chain
            --
            -- Note that invalidation does not affect these numbers (no
            -- decrementation)
            
            Total_Tables           : Natural            := 1;
            -- Gives the total number of tables allocated for this map
            
            Saturation_High_Water  : Natural            := 0;
            -- Gives the numer of contigious tables from the first table
            -- to be fully saturated. This value gives indications into
            -- how efficient registration is for very large or complicated
            -- Codecs (particularily for Path_Maps)
            
            Full_Collisions        : Natural            := 0;
            -- Registrations where the full hash value is an exact match,
            -- but the hashed name differs. Hopefully this never goes
            -- above zero.
         end record;
      
      function Performance (Map: Node_Hash_Map) return Performance_Counters;
      
      
   private
      -- See the package body for more detailed discussion of the
      -- Node_Hash_Maps architecture
      
      type Node_Hash_Table;
      type Node_Hash_Table_Access is access Node_Hash_Table with
        Storage_Pool => Slab_Pool;
      
      type Node_Hash_Map is
         record
            Subpool             : Subpool_Handle                  := null;
            Table_Chain         : Node_Hash_Table_Access          := null;
            
            First_Unsaturated   : Node_Hash_Table_Access          := null;
            Unsaturated_Sequence: Positive                        := 1;
            -- Points to the first Table that is not yet saturated. This is
            -- used for registration only, to save time from needing to scan
            -- saturated tables, which will tend to be towards the start.
            -- The sequence is the number of the table in the chain, which
            -- is used to determine if a Primary (odd) or Secondary (even)
            -- strategy should be used for that table.
            
            Expected_ID         : Slab_Strings.Slab_String_Access := null;
            Candidate_ID        : Slab_Strings.Slab_String_Access := null;
            -- These values are used for name-based lookups, wither for
            -- Path_Maps and Name_Maps. These are access values
            -- to allow use even if Node_Hash_Map is being accessed with
            -- a constant view. These are allocated during Setup.
            
            Profile        : Performance_Counters;
         end record;
      
   end Node_Hash_Maps;
   
   ------------------------
   -- JSON_Value (Nodes) --
   ------------------------
   
   type Value_Container (Kind: JSON_Value_Kind := JSON_Null) is
      record
         case Kind is
            when JSON_Object =>
               Name_Map: Node_Hash_Maps.Node_Hash_Map;
               
               First_Member: Node := null;
               Last_Member : Node := null;
               
               Member_Count: Natural := 0;
               
            when JSON_Array =>
               Index_Map: Node_Hash_Maps.Node_Hash_Map;
               
               First_Element: Node := null;
               Last_Element : Node := null;
               
               Element_Count: Natural := 0;
               
            when JSON_String =>
               String_Value: Slab_Strings.Slab_String;
               
            when JSON_Integer =>
               Integer_Value: JSON_Integer_Value;
               
            when JSON_Float =>
               Float_Value: JSON_Float_Value;
               
            when JSON_Boolean =>
               Boolean_Value: Boolean;
               
            when JSON_Null =>
               null;
         end case;
      end record;
   
   
   type JSON_Value is tagged limited
      record
         Name: Slab_Strings.Slab_String;
         -- Name only applies if the value is a direct child of a JSON_Object
         
         Index: Natural;
         
         Root, Parent, Next, Prev: Node := null;
         -- Root always points at the Root node of the entire tree
         -- (Codec-specific) to which this node belongs. This is used to
         -- verify ownership.
         --
         -- For the actual root JSON_Value, Parent will be null, and Root
         -- will be self-referencing (!)
         --
         -- In general, Root is not used for dereferencing, only for ownership
         -- checking.
         
         Codec_Subpool: Subpool_Handle := null;
         -- Copied-in from the same-named property of the owning Codec on
         -- creation. See that property of Unbounded_JSON_Codec below for
         -- more details
         
         Container: Value_Container;
         -- The JSON_Value type itself is tagged for greater user convenience.
         -- However this means that we cannot give a non-limited full view.
         --
         -- At the same time, we'd like the actual underlying value to be
         -- mutable.
         --
         -- Keeping the public view limited is crucial for the ability to
         -- directly use in-codec data without needing to copy the values,
         -- which is a property we want to keep.
         --
         -- By including this mutable container type to contain the actual
         -- value, we are able to get everything to work as we'd like
      end record;
   
   --------------------------
   -- Unbounded_JSON_Codec --
   --------------------------
   
   package Parsers is new JSON.Parser_Machine 
     (JSON_String_Buffer        => Slab_Strings.Slab_String,
      JSON_String_Buffer_Config => Subpool_Handle,
      Setup_Buffer              => Slab_Strings.Setup,
      Clear                     => Slab_Strings.Clear,
      Append                    => Slab_Strings.Append,
      Export                    => Slab_Strings.Transfer,
      String_Chunk_Size         => Slab_Strings.Chunk_Size);
     
   
   type Unbounded_JSON_Codec is new Ada.Finalization.Limited_Controlled with
      record
         Write_Only   : Boolean        := False;
         -- If true, all hashing on append is disabled, and all
         -- hash-based lookup causes Constraint_Error
         
         Codec_Subpool: Subpool_Handle := null; 
         -- For this design, we use a single subpool per Codec, which is
         -- used to allocate all nodes, hash tables, and Slab_Strings.
         
         Path_Buffer  : Slab_Strings.Slab_String;
         -- When appending items to any structure in this codec, a Full_Path
         -- needs to be computed so that the path can be added to the Path_Map
         --
         -- Since Slab_Strings can never have their memory reclaimed until the
         -- entire Codec is finalized, we put this one here in the Codec as
         -- a kind of scratch buffer to quickly build-out the path when needed.
         --
         -- Since Slab_Strings are lazily allocated, if this never gets used,
         -- that's fine too!
         
         Path_Map     : Node_Hash_Maps.Node_Hash_Map;
         Root_Node    : Node;
         -- Root_Node is initialized as a JSON_Object
         
         Parser           : Parsers.Parser_FSM;
         Current_Structure: Node;
         -- Current_Structure is a bit of state that the FSM contracts out to
         -- the operator (the Codec), who has a bigger picture, and is able
         -- to easily know what kind of structure the parent structure is.
         -- The FSM uses this to detect invalid JSON more immediately 
         -- (specifically object members missing names, or array elements
         -- having names.
         --
         -- When the Codec is initialized, this component equals Root_Node
         
         Error: Slab_Strings.Slab_String;
         -- Contains a detailed error message when deserialization failed
         -- to properly initialize the Codec
         
         End_Error: Boolean := False;
         -- Is set to True if an Ada.IO_Exceptions.End_Error is raised during
         -- a stream deserialization
      end record;
   
   overriding
   procedure Initialize (Codec: in out Unbounded_JSON_Codec);
   
   overriding
   procedure Finalize (Codec: in out Unbounded_JSON_Codec);

   
   type Unbounded_FSM_JSON_Codec is limited new Unbounded_JSON_Codec with
     null record;
   
end JSON.Unbounded_Codecs;
