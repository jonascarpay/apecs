{ mkDerivation, apecs, base, Cabal, containers, inline-c, linear
, stdenv, template-haskell, vector
}:
mkDerivation {
  pname = "apecs-physics";
  version = "0.4.0";
  src = ./.;
  setupHaskellDepends = [ base Cabal ];
  libraryHaskellDepends = [
    apecs base containers inline-c linear template-haskell vector
  ];
  homepage = "https://github.com/jonascarpay/apecs-physics#readme";
  description = "2D physics for apecs";
  license = stdenv.lib.licenses.bsd3;
}
