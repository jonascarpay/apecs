let
  haskellNix =
    let
      # 2021-04-13
      commit = "0057d59cffdaed7fa7475f9d6e9a6e84064b6870";
      sha256 = "07wsgvaarvbl043nwpliq7mygasa16lrrwnc5nyz2j8anpdd4jq4";
    in
    import
      (builtins.fetchTarball {
        url = "https://github.com/input-output-hk/haskell.nix/archive/${commit}.tar.gz";
        inherit sha256;
      })
      { };
  # It might be worth setting this to a more stable channel, but see https://github.com/jonascarpay/template-haskell/issues/9
  pkgsSrc = haskellNix.sources.nixpkgs-unstable;
  pkgsArgs = haskellNix.nixpkgsArgs;
  overlay = self: _: {
    hsPkgs = self.haskell-nix.project {
      src = self.haskell-nix.haskellLib.cleanGit {
        src = ./.;
        name = "apecs";
      };
      compiler-nix-name = "ghc8104";
    };
  };
in
import pkgsSrc (pkgsArgs // {
  overlays = pkgsArgs.overlays ++ [ overlay ];
})
