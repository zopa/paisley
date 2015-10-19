{ pkgs ? (import <nixpkgs> {}).pkgs
, hask ? (import <nixpkgs> {}).haskellPackages
}:

let 
paisley-core = hask.callPackage ../core {};

env = hask.ghcWithPackages (p: with p; [
  acid-state
  cabal-install
  cereal-text
  snap
  time
  lens
  wreq
  stripe
  paisley-core
]);

in pkgs.myEnvFun {
    name = "paisley";
    buildInputs = [ env ];
}
