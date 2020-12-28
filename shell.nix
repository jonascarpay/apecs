let
  pkgs = import ./pkgs.nix;
  hsPkgs = pkgs.hsPkgs;
  ormolu = pkgs.haskell-nix.tool hsPkgs.projectArgs.compiler-nix-name "ormolu" "0.1.4.1";
  # Warning: only works with ghc 8.10 and up!
  ormolu-wrapped = pkgs.writeShellScriptBin "ormolu" ''
    ${ormolu}/bin/ormolu --ghc-opt=-XImportQualifiedPost $@
  '';
in
hsPkgs.shellFor {
  withHoogle = true;
  tools = {
    cabal = "3.2.0.0";
    hlint = "3.2.3";
    ghcid = "0.8.7";
    haskell-language-server = "0.6.0";
  };
  buildInputs = [ ormolu-wrapped ];
  exactDeps = true;
}
