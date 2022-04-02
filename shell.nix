let
  stable = import
    (builtins.fetchTarball {
      name = "nixos-21.11";
      url = "https://github.com/NixOS/nixpkgs/archive/21.11.tar.gz";
      sha256 = "162dywda2dvfj1248afxc45kcrg83appjd0nmdb541hl7rnncf02";
    })
    { };
in
stable.mkShell {
  buildInputs =
    [
      stable.cabal-install
      stable.hpack
      (stable.haskell.packages.ghc8107.ghcWithPackages (p: [
        p.zlib
        p.postgresql-libpq
      ]))

      stable.haskell-language-server
      stable.haskellPackages.hspec-discover
      stable.haskellPackages.ormolu

      stable.libdeflate
      stable.fzf
      stable.zlib.dev
      stable.zlib-ng
      stable.libpqxx
      stable.postgresql
    ];
}

