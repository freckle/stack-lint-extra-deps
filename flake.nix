{
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "nixos";
      repo = "nixpkgs";
      ref = "nixos-unstable";
    };
    stacklock2nix = {
      type = "github";
      owner = "cdepillabout";
      repo = "stacklock2nix";
    };
    pgp-wordlist = {
      type = "github";
      owner = "quchen";
      repo = "pgp-wordlist";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, pgp-wordlist, stacklock2nix }:
    let
      supportedSystems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);

      nixpkgsFor =
        forAllSystems (system: import nixpkgs {
          inherit system;
          overlays = [
            stacklock2nix.overlay
            self.overlays.default
          ];
        });
    in
    {
      # All packages provided by this flake
      packages = forAllSystems (system: rec {
        default = stack-lint-extra-deps;
        stack-lint-extra-deps = nixpkgsFor.${system}.stack-lint-extra-deps;
      });

      overlays.default = final: prev: {
        stack-lint-extra-deps-stacklock = final.stacklock2nix {

          stackYaml = ./stack.yaml;

          # GHC version that matches stack.yaml
          baseHaskellPkgSet = final.haskell.packages.ghc966;

          all-cabal-hashes = final.fetchFromGitHub {
            owner = "commercialhaskell";
            repo = "all-cabal-hashes";
            rev = "69c9ea6a7746281865968fdccf00a07f5e1bdc04";
            sha256 = "sha256-VMUGBZGaiyDrikZh/t2/M7QKekOkK1auPtK2KmnakZ8=";
          };

          additionalHaskellPkgSetOverrides = hfinal: hprev: {
            # Workaround for unreleased updates
            pgp-wordlist =
              final.haskell.lib.dontCheck (hprev.callCabal2nix "pgp-wordlist" pgp-wordlist { });
          } //
          # Workarounds for issues in tests
          nixpkgs.lib.genAttrs [
            "ansi-wl-pprint"
            "case-insensitive"
            "integer-logarithms"
            "lifted-base"
            "prettyprinter"
            "prettyprinter-compat-ansi-wl-pprint"
            "primitive"
            "quickcheck-instances"
            "test-framework"
            "uuid-types"
            "yaml-marked"
          ]
            (name: final.haskell.lib.dontCheck hprev.${name});
        };

        stack-lint-extra-deps =
          final.stack-lint-extra-deps-stacklock.pkgSet.stack-lint-extra-deps;
      };
    };

  nixConfig = {
    extra-substituters = [
      "https://freckle.cachix.org"
    ];
    extra-trusted-public-keys = [
      "freckle.cachix.org-1:WnI1pZdwLf2vnP9Fx7OGbVSREqqi4HM2OhNjYmZ7odo="
    ];
  };
}
