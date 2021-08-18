
-- JSON AURA Configuration Manifest

package JSON.AURA is
   
   package Configuration is
      Truncate_Floats: constant Boolean := False;
      -- If true, when deserializing a JSON objects, if the read value for the
      -- JSON_Float_Value type has too many decimal points, the deserialized
      -- value is truncated (not rounded) past the maximum number of deciaml
      -- points. Otherwise (when set False), the parser will abort.
      --
      -- Note that exponentiated values will only have the fractional part
      -- truncated, and if the exponent is still too large, the parser will
      -- still abort
      
      Unbounded_Codec_Slab_Size     : constant := 1024 ** 2; -- 1 MiB slab
      Unbounded_Codec_Slab_Alignment: constant := 4096;      -- 4 KiB page
      
      -- Slab_Size should be a multiple of an appropriate page size for the
      -- target system, but should be at least 1 MiB in size.
      --
      -- Alignment should align the slabs on the appropriate page boundaries
      --
      -- Currently, 1 MiB slabs with 4 KiB alignment is optimal for basically
      -- every target architecture this can run on.
      
      type Hash_Algorithm is (xxHash32, xxHash64);
      Unbounded_Codec_Hash_Algo: constant Hash_Algorithm := xxHash64;
      
      -- The selected hash should depend on the word-size of the machine this
      -- is executing on
      
   end Configuration;
   
end JSON.AURA;
