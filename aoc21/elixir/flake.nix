{
  description = "Elixir/phoenix setup";

  # Inputs
  # https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html#flake-inputs
  # The nixpkgs entry in the flake registry.
  inputs = { emacsConfig.url = "github:sstoltze/emacs-config"; };

  outputs = { emacsConfig, ... }: { devShells = emacsConfig.elixirDevShells; };
}
