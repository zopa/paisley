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
  license = {
    fullName = "Unreleased";
    free = true; # Stop yelling at me about allowUnfree
  };
  postBuild = ''
    mkdir -p $out/css
    cp -r ./css/* $out/css
  '';  
}
