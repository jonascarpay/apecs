let
  haskellNix =
    let
      # 2020-12-21
      commit = "67ab92fc6d76a9ef12f49f83dd1f19665d5445d7";
      sha256 = "0985zdsx434ykaj44ynnd73qsjl18l68172qgnwb7j1q82wgldsw";
    in
    import
      (builtins.fetchTarball {
        url = "https://github.com/input-output-hk/haskell.nix/archive/${commit}.tar.gz";
        inherit sha256;
      })
      { };
  pkgsSrc = haskellNix.sources.nixpkgs-2009;
  pkgsArgs = haskellNix.nixpkgsArgs;
  overlay = self: _: {
    hsPkgs = self.haskell-nix.project {
      src = self.haskell-nix.haskellLib.cleanGit {
        name = "apecs";
        src = ./.;
      };
      projectFileName = "cabal.project";
      compiler-nix-name = "ghc8102";
    };
  };
in
import pkgsSrc (pkgsArgs // {
  overlays = pkgsArgs.overlays ++ [ overlay ];
})
