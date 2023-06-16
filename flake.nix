{
  description = "apecs";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              apecs = hfinal.callCabal2nix "apecs" ./apecs { };
              apecs-stm = hfinal.callCabal2nix "apecs-stm" ./apecs-stm { };
              apecs-physics = hfinal.callCabal2nix "apecs-physics" ./apecs-physics { };
              apecs-gloss = hfinal.callCabal2nix "apecs-gloss" ./apecs-gloss { };
              examples = hfinal.callCabal2nix "examples" ./examples { };
            };
        };
        examples = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.examples;
      };

      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          hspkgs = pkgs.haskellPackages;
        in
        {
          devShell = hspkgs.shellFor {
            withHoogle = true;
            packages = p: [
              p.apecs
              p.apecs-stm
              p.apecs-physics
              p.apecs-gloss
              p.examples
            ];
            buildInputs = [
              hspkgs.cabal-install
              hspkgs.haskell-language-server
              hspkgs.hlint
              hspkgs.ormolu
              pkgs.bashInteractive
            ];
          };
          defaultPackage = pkgs.examples;
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
