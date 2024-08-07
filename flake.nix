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

  outputs =
    {
      self,
      nixpkgs,
      pgp-wordlist,
      stacklock2nix,
    }:
    let
      supportedSystems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);

      nixpkgsFor = forAllSystems (
        system:
        import nixpkgs {
          inherit system;
          overlays = [
            stacklock2nix.overlay
            self.overlays.default
          ];
        }
      );
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

          # It is necessary to get this using a fetcher that doesn't unpack to
          # preserve hash compatibility among case (in/)sensitive file systems.
          all-cabal-hashes = final.fetchurl {
            name = "all-cabal-hashes";
            url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/69c9ea6a7746281865968fdccf00a07f5e1bdc04.tar.gz";
            sha256 = "1l9k4sg7pigr73749h4nkldllvl36d86sghjb5ibqpckkrbr3yky";
          };

          additionalHaskellPkgSetOverrides =
            hfinal: hprev:
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
            ] (name: final.haskell.lib.dontCheck hprev.${name});

          additionalDevShellNativeBuildInputs = stacklockHaskellPkgSet: [
            final.cabal-install
            final.haskellPackages.fourmolu
            final.stack
            final.zlib
          ];
        };

        stack-lint-extra-deps = final.stack-lint-extra-deps-stacklock.pkgSet.stack-lint-extra-deps;
      };

      devShells = forAllSystems (system: {
        default = nixpkgsFor.${system}.stack-lint-extra-deps-stacklock.devShell;
      });
    };

  nixConfig = {
    extra-substituters = [ "https://freckle.cachix.org" ];
    extra-trusted-public-keys = [ "freckle.cachix.org-1:WnI1pZdwLf2vnP9Fx7OGbVSREqqi4HM2OhNjYmZ7odo=" ];
  };
}
