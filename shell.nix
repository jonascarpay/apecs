let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          apecs         = haskellPackagesNew.callPackage ./apecs/default.nix         {};
          apecs-physics = haskellPackagesNew.callPackage ./apecs-physics/default.nix {};
          apecs-gloss   = haskellPackagesNew.callPackage ./apecs-gloss/default.nix   {};
          examples      = haskellPackagesNew.callPackage ./examples/default.nix      {};
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
  hpkgs = pkgs.haskellPackages;

  f = { mkDerivation, stdenv, apecs, apecs-physics, apecs-gloss, examples }:
    mkDerivation {
      pname = "apecs-suite";
      version = "1";
      license = stdenv.lib.licenses.bsd3;
      libraryHaskellDepends = [
        apecs
        apecs-physics
        apecs-gloss
        examples
      ];
      src = ./.;
    };

in
  (hpkgs.callPackage f {}).env
