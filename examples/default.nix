{ mkDerivation, apecs, apecs-gloss, apecs-physics, base, gloss
, linear, random, stdenv
}:
mkDerivation {
  pname = "examples";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    apecs apecs-gloss apecs-physics base gloss linear random
  ];
  homepage = "https://github.com/jonascarpay/apecs/examples#readme";
  description = "examples for apecs";
  license = stdenv.lib.licenses.bsd3;
}
