let
  haskellNixSrc = builtins.fetchTarball {
    url =
      "https://github.com/input-output-hk/haskell.nix/archive/fd60cf3f07d8e3c01feed62b69ea944c5fa067e7.tar.gz";
    sha256 = "090hmx3ppzbf2f59m9y9b8nvsrqbgrfvm1jj2630v6x72vh17wpa";
  };
  haskellNix = import haskellNixSrc { };
  pkgsSrc = haskellNix.sources.nixpkgs-2003;
  pkgsArgs = haskellNix.nixpkgsArgs;
  pkgs = import pkgsSrc pkgsArgs;
in pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
  compiler-nix-name = "ghc883";
}
