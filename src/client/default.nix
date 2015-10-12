{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:
let callPackage = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage;
    paisley-core = callPackage ../core { compiler = "ghcjs"; };
in callPackage ./paisley-client.nix { }
