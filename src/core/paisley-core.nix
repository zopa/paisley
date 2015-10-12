{ mkDerivation, acid-state, base, cereal, cereal-text, containers
, lens, mtl, safecopy, stdenv, stripe, syb, time
}:
mkDerivation {
  pname = "paisley-core";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    acid-state base cereal cereal-text containers lens mtl safecopy
    stripe syb time
  ];
  license = stdenv.lib.licenses.unfree;
}
