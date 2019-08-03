{ mkDerivation, base, containers, criterion, linear, mtl
, QuickCheck, stdenv, template-haskell, vector
}:
mkDerivation {
  pname = "apecs";
  version = "0.8.1";
  src = ./.;
  libraryHaskellDepends = [
    base containers mtl template-haskell vector
  ];
  testHaskellDepends = [
    base containers criterion linear QuickCheck vector
  ];
  benchmarkHaskellDepends = [ base criterion linear ];
  homepage = "https://github.com/jonascarpay/apecs#readme";
  description = "Fast ECS framework for game programming";
  license = stdenv.lib.licenses.bsd3;
}
