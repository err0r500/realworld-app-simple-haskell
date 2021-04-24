let                                
   stable = import (builtins.fetchTarball {                                
      name = "nixos-20.09";                                
      url = "https://github.com/NixOS/nixpkgs/archive/20.09.tar.gz";                                
      sha256 = "1wg61h4gndm3vcprdcg7rc4s1v3jkm5xd7lw8r2f67w502y94gcy";                                
   }) {};                                
  unstable = import (builtins.fetchTarball https://nixos.org/channels/nixos-unstable/nixexprs.tar.xz) { };
  in                                
  stable.mkShell {                                
    buildInputs =                                
       [ 
         unstable.cabal-install
         unstable.hpack
         unstable.ghc
         unstable.haskell-language-server
         unstable.haskellPackages.hspec-discover
         unstable.haskellPackages.ormolu

         stable.libdeflate
         stable.fzf
         stable.zlib
         stable.libpqxx
         stable.postgresql
     ];                                
  }      


