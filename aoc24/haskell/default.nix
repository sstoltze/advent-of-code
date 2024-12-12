{ lib, haskell, haskellPackages, ... }:
lib.pipe (haskellPackages.callCabal2nix "aoc24" (lib.cleanSource ./.) { })
  [ haskell.lib.compose.dontHaddock ]
