{ mkDerivation, acid-state, base, bytestring, cereal, containers
, lens , mtl, paisley-core, snap-core, snap-server, stdenv, stripe
, text, transformers
}:
mkDerivation {
  pname = "paisley-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    acid-state base bytestring cereal containers lens mtl
    paisley-core snap-core snap-server stripe text transformers
  ];
  license = {
    fullName = "Unreleased";
    free = true; # Stop yelling at me about allowUnfree
  };
}
