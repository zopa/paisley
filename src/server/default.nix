{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
let callPackage = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage;
    paisley-core = callPackage ../core {};
in callPackage ./paisley-server.nix { inherit paisley-core; }
