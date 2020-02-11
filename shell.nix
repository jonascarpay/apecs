let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: rec {
          apecs         = self.callCabal2nix "apecs"         ./apecs         {};
          apecs-stm     = self.callCabal2nix "apecs-stm"     ./apecs-stm     {};
          apecs-physics = self.callCabal2nix "apecs-physics" ./apecs-physics {};
          apecs-gloss   = self.callCabal2nix "apecs-gloss"   ./apecs-gloss   {};
          examples      = self.callCabal2nix "examples"      ./examples      {};
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
  hpkgs = pkgs.haskellPackages;

  f = { mkDerivation, stdenv, ghcid, hlint
      , apecs, apecs-stm, apecs-physics, apecs-gloss, examples }:
    mkDerivation {
      pname = "apecs-suite";
      version = "1";
      license = stdenv.lib.licenses.bsd3;
      libraryHaskellDepends = [
        ghcid
        hlint
        apecs
        # apecs-stm # 1.1.0.4 is marked as broken
        apecs-physics
        apecs-gloss
        examples
      ];
      src = ./.;
    };

in
  (hpkgs.callPackage f {}).env
