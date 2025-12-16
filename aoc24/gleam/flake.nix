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
      devShells = forEachSystem (
        { pkgs, ... }:
        {
          default = pkgs.mkShell {

            packages =
              with pkgs;
              [
                gleam
                erlang_27
                rebar3
              ]
              ++ lib.optional stdenv.isLinux inotify-tools
              ++ lib.optional stdenv.isDarwin terminal-notifier
              ++ lib.optional stdenv.isDarwin fswatch;
          };
        }
      );
    };
}
