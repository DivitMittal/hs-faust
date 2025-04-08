{
  description = "hs-cabal";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    systems.url = "github:nix-systems/default";
    devshell = {
      url = "github:numtide/devshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pre-commit-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {flake-parts, ...} @ inputs:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = import inputs.systems;
      imports = [
        inputs.devshell.flakeModule
        inputs.treefmt-nix.flakeModule
        ./pre-commit.nix
      ];

      perSystem = {pkgs, ...}: {
        devshells = {
          default = {
            packages = builtins.attrValues {
              inherit
                (pkgs)
                ## Haskell
                cabal-install
                ghc
                haskell-language-server
                stylish-haskell

                ## C/C++
                clang-tools
                ;
              inherit (pkgs.haskellPackages) cabal-gild;
            };
          };
        };
        treefmt = {
          flakeCheck = false;
          programs = {
            stylish-haskell.enable = true;
            clang-format.enable = true;
            #typos.enable = true;
          };
          projectRootFile = "flake.nix";
        };
      };
    };
}