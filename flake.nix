{
  description = "heed";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  outputs = { self, nixpkgs, flake-utils, haskellNix, flake-compat }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            heed-vty = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc924";
              shell.tools = {
                cabal = { };
                haskell-language-server = { };
              };
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.heed-vty.flake {
        };
      in flake // {
        # Built by `nix build .`
        defaultPackage = flake.packages."heed-vty:exe:heed-vty";
      });
}
