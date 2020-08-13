let

  haskellNixSrc = (import <nixpkgs> {}).fetchFromGitHub {
    repo = "haskell.nix";
    owner = "input-output-hk";
    rev = "c7c7d6c43af27a632f16e631202eb83ac3c047c3"; # master 11082020
    sha256 = "0xrfl0zwf98cyv6px0awblhff97vjv19a5mdvs6l98769qgh4558";
  };

  haskellNix = import haskellNixSrc {};

  all-hies = (import <nixpkgs> {}).fetchFromGitHub {
    repo = "all-hies";
    owner = "infinisil";
    rev = "534ac517b386821b787d1edbd855b9966d0c0775"; # master 12082020
    sha256 = "0bw1llpwxbh1dnrnbxkj2l0j58s523hjivszf827c3az5i4py1i2";
  };

  pkgs = import haskellNix.sources.nixpkgs-2003 (haskellNix.nixpkgsArgs // {
    crossSystem = haskellNix.pkgs.lib.systems.examples.musl64;
    overlays = haskellNix.nixpkgsArgs.overlays ++ [
      (import all-hies {}).overlay
    ];
  });

in pkgs.haskell-nix.cabalProject {
  name = "mqtt-clean-retain";
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "mqtt-clean-retain";
    src = ./.;
  };
  # ghc = pkgs.haskell-nix.compiler.ghc865;
  compiler-nix-name = "ghc865";
  configureFlags =
    pkgs.lib.optionals pkgs.hostPlatform.isMusl [
      "--disable-executable-dynamic"
      "--disable-shared"
      "--ghc-option=-optl=-pthread"
      "--ghc-option=-optl=-static"
      "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
      "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
    ];
}
