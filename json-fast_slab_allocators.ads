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

-- GNAT Bug Workaround --
--
-- This package spec is supposed to be nested in the private part of
-- JSON.Unbounded_Codec (with body as a separate compilation), GNAT can't
-- get itself to accept creating an access type using this storage pool if
-- the package is nested.
--
-- Not only that, if the storage pool object itself is declared in the same
-- package as the access type, it also crashes GNAT.

with System.Storage_Elements;
with System.Storage_Pools.Subpools;

private package JSON.Fast_Slab_Allocators is
   use System.Storage_Elements;
   use System.Storage_Pools.Subpools;
   
   type Slab_Pool_Designator is new Root_Storage_Pool_With_Subpools
     with null record;

   -- All subpools are functionally independent, allocating storage from
   -- the Standard Storage Pool. Therefore all objects of
   -- Slab_Pool_Designator are functionally identical
   
   overriding
   function Create_Subpool (Pool: in out Slab_Pool_Designator) 
                           return not null Subpool_Handle;
   
   -- Note that individual subpools are NOT task-safe. Though if the
   -- Standard Storage Pool is task-safe (usually it is), Create_Subpool
   -- itself will be task-safe vis-a-vis any Slab_Pool_Designator
   
   overriding
   procedure Allocate_From_Subpool
     (Pool                    : in out Slab_Pool_Designator;
      Storage_Address         :    out System.Address;
      Size_In_Storage_Elements: in     Storage_Count;
      Alignment               : in     Storage_Count;
      Subpool                 : in     not null Subpool_Handle);
   
   overriding
   procedure Deallocate_Subpool (Pool   : in out Slab_Pool_Designator;
                                 Subpool: in out Subpool_Handle);
   
   
   -- GNAT Bug Workaround --
   -- Turns out that the actual pool object itself can't be declared outside
   -- of the below package without crashing GNAT. So it has been placed
   -- inside the package spec, until this issue can be resolved.
   --
   -- This also means that Slab_Pool_Designator needs to be either a public
   -- record, or a null record. Thankfully it is designed as a null record.
   
   Global_Slab_Pool: Slab_Pool_Designator;
   
   -- Normally such a storage pool would be allocated outside of this package,
   -- but GNAT can't handle that apparently. Luck for us, this is just a null
   -- record.
   
end JSON.Fast_Slab_Allocators;
