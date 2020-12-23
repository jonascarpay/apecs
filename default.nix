# This nix derivation builds all apecs-related packages.
# This is primarily intended for CI, and not of a lot of practical value to most people.
# For regular hacking, I personally use cabal from the nix-shell, but stack works too.
let
  pkgs = import ./pkgs.nix;
  inherit (pkgs) lib;
  localPkgs = lib.filterAttrs (_: pkg: pkg.isLocal or false) (pkgs.hsPkgs);
  collectBuildables = pkg:
    let
      comps = pkg.components;
      library = lib.optional (builtins.hasAttr "library" comps) comps.library;
      exes = builtins.attrValues comps.exes;
      tests = builtins.attrValues comps.tests;
      benches = builtins.attrValues comps.benchmarks;
      checks = builtins.attrValues pkg.checks;
    in
    library ++ exes ++ tests ++ benches;
in
pkgs.symlinkJoin {
  name = "apecs-suite";
  paths = (lib.concatMap collectBuildables (builtins.attrValues localPkgs));
}
