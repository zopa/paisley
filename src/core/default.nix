{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
nixpkgs.pkgs.haskell.packages.ghcjs.callPackage ./paisley-core.nix { }
