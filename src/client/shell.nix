{ pkgs ? (import <nixpkgs> {}).pkgs
, hask ? (import <nixpkgs> {}).pkgs.haskell.packages.ghcjs
}:

let 

env = hask.ghcWithPackages (p: with p; [
  text
  data-default
  file-embed
  lens
  reflex
  reflex-dom
]);

in pkgs.myEnvFun {
    name = "paisley";
    buildInputs = [ env ];
}
