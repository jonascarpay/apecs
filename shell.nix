let
  pkgs = import ./pkgs.nix;
  hsPkgs = pkgs.hsPkgs;
  ## To configure ormolu, I often end up creating a wrapped version in which I pass it the desired flags.
  ## To use it, remove the ormolu line from the `tools` section, uncomment the lines below, and add `ormolu-wrapped` to  `shellFor`'s `buildInputs`.
  # ormolu-wrapped =
  #   let ormolu = pkgs.haskell-nix.tool hsPkgs.projectModule.compiler-nix-name "ormolu" "latest";
  #   in
  #   pkgs.writeShellScriptBin "ormolu" ''
  #     ${ormolu}/bin/ormolu --ghc-opt=-XImportQualifiedPost $@
  #   '';
in
hsPkgs.shellFor {
  withHoogle = true;
  tools = {
    cabal = "latest";
    ghcid = "latest";
    haskell-language-server = "latest";
    ormolu = "latest";
    hlint = "latest";
  };
  # buildInputs = [ ormolu-wrapped ]; # See note about ormolu-wrapped above
  exactDeps = true;
}
