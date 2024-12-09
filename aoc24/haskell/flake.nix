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
      # nix-shell -p 'haskellPackages.ghcWithPackages (pkgs: with pkgs; [ cabal-install ])'
      # cabal update
      # cabal init
      devShells = forEachSystem (
        { pkgs, ... }:
        {
          default = pkgs.mkShell {
            packages = [ (pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [ cabal-install ])) ];
            inputsFrom = [
              (pkgs.haskellPackages.developPackage {
                root = ./.;
                returnShellEnv = true;
              })
            ];
          };
        }
      );
    };
}
