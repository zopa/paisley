{ pkgs ? (import <nixpkgs> {}).pkgs
, hask ? (import <nixpkgs> {}).haskellPackages
}:

let env = hask.ghcWithPackages (p: with p; [
  acid-state
  cabal-install
  cereal-text
  snap
  time
  lens
  wreq
  stripe
]);

in pkgs.myEnvFun {
    name = "tm";
    buildInputs = [ env ];
}
