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

separate (JSON.Unbounded_Codecs.Node_Hash_Maps)

package body Indexing_Strategies is
   
   package MH renames Modular_Hashing;
   
   type Indexing_Tactic is not null access 
     function (Bin: MH.Hash_Binary_Value) return Match_Table_Index;
   
   type Indexing_Tactics is array (Config.Hash_Algorithm, Match_Level) of
     Indexing_Tactic;
   
   -- Primary_Tactics --
   
   function XXHL32_PRI_L1 (Bin: MH.Hash_Binary_Value) return Match_Table_Index;
   function XXHL32_PRI_L2 (Bin: MH.Hash_Binary_Value) return Match_Table_Index;
   function XXHL32_PRI_L3 (Bin: MH.Hash_Binary_Value) return Match_Table_Index;
   function XXHL32_PRI_L4 (Bin: MH.Hash_Binary_Value) return Match_Table_Index;
   
   function XXHL64_PRI_L1 (Bin: MH.Hash_Binary_Value) return Match_Table_Index;
   function XXHL64_PRI_L2 (Bin: MH.Hash_Binary_Value) return Match_Table_Index;
   function XXHL64_PRI_L3 (Bin: MH.Hash_Binary_Value) return Match_Table_Index;
   function XXHL64_PRI_L4 (Bin: MH.Hash_Binary_Value) return Match_Table_Index;
   
   Primary_Tactics: constant Indexing_Tactics
     := (xxHash32 => (1 => XXHL32_PRI_L1'Access,
                      2 => XXHL32_PRI_L2'Access,
                      3 => XXHL32_PRI_L3'Access,
                      4 => XXHL32_PRI_L4'Access),
         
         xxHash64 => (1 => XXHL64_PRI_L1'Access,
                      2 => XXHL64_PRI_L2'Access,
                      3 => XXHL64_PRI_L3'Access,
                      4 => XXHL64_PRI_L4'Access));
   
   -- Secondary_Tactics --
   
   function XXHL32_SEC_L1 (Bin: MH.Hash_Binary_Value) return Match_Table_Index;
   function XXHL32_SEC_L2 (Bin: MH.Hash_Binary_Value) return Match_Table_Index;
   function XXHL32_SEC_L3 (Bin: MH.Hash_Binary_Value) return Match_Table_Index;
   function XXHL32_SEC_L4 (Bin: MH.Hash_Binary_Value) return Match_Table_Index;
   
   function XXHL64_SEC_L1 (Bin: MH.Hash_Binary_Value) return Match_Table_Index;
   function XXHL64_SEC_L2 (Bin: MH.Hash_Binary_Value) return Match_Table_Index;
   function XXHL64_SEC_L3 (Bin: MH.Hash_Binary_Value) return Match_Table_Index;
   function XXHL64_SEC_L4 (Bin: MH.Hash_Binary_Value) return Match_Table_Index;
   
   Secondary_Tactics: constant Indexing_Tactics
     := (xxHash32 => (1 => XXHL32_SEC_L1'Access,
                      2 => XXHL32_SEC_L2'Access,
                      3 => XXHL32_SEC_L3'Access,
                      4 => XXHL32_SEC_L4'Access),
         
         xxHash64 => (1 => XXHL64_SEC_L1'Access,
                      2 => XXHL64_SEC_L2'Access,
                      3 => XXHL64_SEC_L3'Access,
                      4 => XXHL64_SEC_L4'Access));
   
   ------------------
   -- Compute_Plan --
   ------------------
   
   generic
      Tactics: in Indexing_Tactics;
   function Generic_Compute_Plan (Hash: Hash_Container) return Indexing_Plan;
   
   function Generic_Compute_Plan (Hash: Hash_Container) return Indexing_Plan
   is 
      Algo: Config.Hash_Algorithm renames Config.Unbounded_Codec_Hash_Algo;
      Hash_Bin: MH.Hash_Binary_Value 
        := (case Hash.Kind is 
               when xxHash32 => Hash.XXH32.Binary,
               when xxHash64 => Hash.XXH64.Binary);
   begin
      
      pragma Assert (Hash_Bin'First = 1);
      pragma Assert (case Algo is 
                        when xxHash32 => Hash_Bin'Length = 4,
                        when xxHash64 => Hash_Bin'Length = 8);
      
      return Plan: Indexing_Plan do
         for Level in Match_Level'Range loop
            Plan(Level) := Tactics(Algo, Level)(Hash_Bin);
         end loop;
      end return;
   end;
   
   
   function Compute_Primary_Plan_Instance is 
     new Generic_Compute_Plan (Primary_Tactics);
   
   function Compute_Primary_Plan (Hash: Hash_Container) return Indexing_Plan
     renames Compute_Primary_Plan_Instance;
   
   
   function Compute_Secondary_Plan_Instance is
     new Generic_Compute_Plan (Secondary_Tactics);
   
   function Compute_Secondary_Plan (Hash: Hash_Container) return Indexing_Plan
     renames Compute_Secondary_Plan_Instance;

   
   --
   -- Strategies
   --
   
   -- Strategy descriptions --

   -- xxHash32
   -- --------
   -- xxHash32 produces a 4-octet (32-bit) value, which matches exactly the
   -- number of Match Levels. The strategy is simple, for the primary tactics,
   -- we will use the octets in order of their index vis-a-vis the match level.
   --
   -- For the secondary tactics, we will simply go in reverse order.

   -- xxHash64
   -- --------
   -- xxHash64 produces an 8-octet (64-bit) value. The XXH64 strategy
   -- interleves odd (primary) and even (secondary) octets per match level.
   
   
   --------------
   -- xxHash32 --
   --------------
   
   function XXHL32_PRI_L1 (Bin: MH.Hash_Binary_Value) return Match_Table_Index 
   is (Match_Table_Index(Bin (1)));
      
   function XXHL32_PRI_L2 (Bin: MH.Hash_Binary_Value) return Match_Table_Index
   is (Match_Table_Index(Bin (2)));

   function XXHL32_PRI_L3 (Bin: MH.Hash_Binary_Value) return Match_Table_Index
   is (Match_Table_Index(Bin (3)));
   
   function XXHL32_PRI_L4 (Bin: MH.Hash_Binary_Value) return Match_Table_Index
   is (Match_Table_Index(Bin (4)));
   
   
   function XXHL32_SEC_L1 (Bin: MH.Hash_Binary_Value) return Match_Table_Index
   is (Match_Table_Index(Bin (4)));
   
   function XXHL32_SEC_L2 (Bin: MH.Hash_Binary_Value) return Match_Table_Index
   is (Match_Table_Index(Bin (3)));
   
   function XXHL32_SEC_L3 (Bin: MH.Hash_Binary_Value) return Match_Table_Index
   is (Match_Table_Index(Bin (2)));
   
   function XXHL32_SEC_L4 (Bin: MH.Hash_Binary_Value) return Match_Table_Index
   is (Match_Table_Index(Bin (1)));
   
   --------------
   -- xxHash64 --
   --------------
   
   function XXHL64_PRI_L1 (Bin: MH.Hash_Binary_Value) return Match_Table_Index
   is (Match_Table_Index(Bin (1)));
   
   function XXHL64_PRI_L2 (Bin: MH.Hash_Binary_Value) return Match_Table_Index
   is (Match_Table_Index(Bin (3)));
   
   function XXHL64_PRI_L3 (Bin: MH.Hash_Binary_Value) return Match_Table_Index
   is (Match_Table_Index(Bin (5)));
   
   function XXHL64_PRI_L4 (Bin: MH.Hash_Binary_Value) return Match_Table_Index
   is (Match_Table_Index(Bin (7)));
   
   
   function XXHL64_SEC_L1 (Bin: MH.Hash_Binary_Value) return Match_Table_Index
   is (Match_Table_Index(Bin (2)));
   
   function XXHL64_SEC_L2 (Bin: MH.Hash_Binary_Value) return Match_Table_Index
   is (Match_Table_Index(Bin (4)));
   
   function XXHL64_SEC_L3 (Bin: MH.Hash_Binary_Value) return Match_Table_Index
   is (Match_Table_Index(Bin (6)));
   
   function XXHL64_SEC_L4 (Bin: MH.Hash_Binary_Value) return Match_Table_Index
   is (Match_Table_Index(Bin (8)));
   
   
end Indexing_Strategies;
