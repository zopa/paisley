{ mkDerivation, base, containers, data-default, file-embed, lens
, reflex, reflex-dom, stdenv, text
}:
mkDerivation {
  pname = "paisley-client";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers data-default file-embed lens reflex reflex-dom text
  ];
  license = stdenv.lib.licenses.unfree;
}
