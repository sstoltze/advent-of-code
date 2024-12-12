{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      forEachSystem = (
        f:
        nixpkgs.lib.genAttrs
          [
            "aarch64-darwin"
            "x86_64-linux"
          ]
          (
            system:
            f {
              inherit system;
              pkgs = import nixpkgs { inherit system; };
            }
          )
      );
    in
    {
      # Based on https://abhinavsarkar.net/posts/nix-for-haskell/
      devShells = forEachSystem
        (
          { pkgs, ... }: { default = pkgs.callPackage ./shell.nix { }; }
        );
      packages = forEachSystem ({ pkgs, ... }: { default = pkgs.callPackage ./. { }; });
    };
}
