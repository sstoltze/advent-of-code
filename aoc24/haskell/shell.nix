{ pkgs, ... }:
let
  haskellWithLocalPackage = pkgs.haskellPackages.extend (final: prev: { aoc24 = pkgs.callPackage ./. { }; });
in
haskellWithLocalPackage.shellFor
{
  packages = p: [ p.aoc24 ];
  nativeBuildInputs = with pkgs;
    [
      ghc
      cabal-install
      niv
      hlint
      ormolu
      (ghc.withPackages (p: [ p.haskell-language-server ]))
    ];
}
