{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, apecs, base, Cabal, containers, inline-c
      , linear, stdenv, template-haskell, vector
      }:
      mkDerivation {
        pname = "apecs-physics";
        version = "0.2.0.0";
        src = ./.;
        setupHaskellDepends = [ base Cabal ];
        libraryHaskellDepends = [
          apecs base containers inline-c linear template-haskell vector pkgs.cabal-install
        ];
        homepage = "https://github.com/jonascarpay/apecs-physics#readme";
        description = "2D physics for apecs";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
