{ mkDerivation, apecs, apecs-physics, base, containers, gloss
, linear, stdenv
}:
mkDerivation {
  pname = "apecs-gloss";
  version = "0.2.2";
  src = ./.;
  libraryHaskellDepends = [
    apecs apecs-physics base containers gloss linear
  ];
  homepage = "https://github.com/jonascarpay/apecs-physics#readme";
  description = "Simple gloss renderer for apecs";
  license = stdenv.lib.licenses.bsd3;
}
