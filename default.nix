{ sources ? import ./nix/sources.nix
, haskellNix ? import sources.haskellNix {}
, pkgs ? import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs
, nix-filter ? import sources.nix-filter
}:
let
    hsPkgs = pkgs.haskell-nix.project {
        src = pkgs.haskell-nix.haskellLib.cleanGit {
          name = "aoc2022";
          src = ./.;
        };

        
        index-state = "2021-11-22T00:00:00Z";

        # Specify the GHC version to use.
        compiler-nix-name = "ghc925"; # Not required for `stack.yaml` based projects.
        
    };
in hsPkgs