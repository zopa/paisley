{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:
let callPackage = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage;
in callPackage ./paisley-client.nix { }
