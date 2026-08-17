{
  description = "apecs";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.box-nd = {
    type = "git";
    url = "https://gitlab.com/dpwiz/box-nd";
    rev = "ca8f1ee8e530fdeb0a0f3689c59d194622e78480";
    submodules = true;
    flake = false;
  };

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
              apecs-gloss-3d = hfinal.callCabal2nix "apecs-gloss-3d" ./apecs-gloss-3d { };
              Box2D = prev.haskell.lib.dontCheck (hfinal.callCabal2nix "Box2D" "${inputs.box-nd}/Box2D" { });
              Box3D = prev.haskell.lib.dontCheck (hfinal.callCabal2nix "Box3D" "${inputs.box-nd}/Box3D" { });
              apecs-box2d = hfinal.callCabal2nix "apecs-box2d" ./apecs-box2d { };
              apecs-box3d = hfinal.callCabal2nix "apecs-box3d" ./apecs-box3d { };
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
              p.apecs-gloss-3d
              p.apecs-box2d
              p.apecs-box3d
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
