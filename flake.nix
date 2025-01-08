{
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "nixos";
      repo = "nixpkgs";
      ref = "nixos-24.11";
    };
    nixpkgs-haskell-updates = {
      type = "github";
      owner = "nixos";
      repo = "nixpkgs";
      ref = "haskell-updates";
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
      nixpkgs-haskell-updates,
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
        nixpkgs-haskell-updates.legacyPackages.${system}.appendOverlays [
          stacklock2nix.overlay
          self.overlays.default
        ]
      );
    in
    {
      # All packages provided by this flake
      packages = forAllSystems (system: rec {
        default = stack-lint-extra-deps;
        stack-lint-extra-deps = nixpkgsFor.${system}.stack-lint-extra-deps;
      });

      checks = forAllSystems (
        system:
        let
          pkgs = nixpkgsFor.${system};
        in
        rec {
          sled-version = pkgs.testers.testVersion {
            package = pkgs.stack-lint-extra-deps;
          };
        }
      );

      overlays.default = final: prev: {
        stack-lint-extra-deps-stacklock = final.stacklock2nix {

          stackYaml = ./stack.yaml;

          # GHC version that matches stack.yaml
          baseHaskellPkgSet = final.haskell.packages.ghc984;

          # It is necessary to get this using a fetcher that doesn't unpack to
          # preserve hash compatibility among case (in/)sensitive file systems.
          all-cabal-hashes = final.fetchurl {
            name = "all-cabal-hashes";
            url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/090338d05bcc279b2c99f0251943a62ef4e5f8ae.tar.gz";
            sha256 = "sha256-DFxLXAXAfZjk+LIwezqpnQGo9GgwI6Fpua31aTOWI+I=";
          };

          additionalHaskellPkgSetOverrides =
            hfinal: hprev:
            # Workarounds for issues in tests
            nixpkgs-haskell-updates.lib.genAttrs [
              "ansi-wl-pprint"
              "case-insensitive"
              "integer-logarithms"
              "lifted-base"
              "prettyprinter"
              "prettyprinter-compat-ansi-wl-pprint"
              "primitive"
              "quickcheck-instances"
              "serialise"
              "test-framework"
              "uuid-types"
              "yaml-marked"
            ] (name: final.haskell.lib.dontCheck hprev.${name});

          additionalDevShellNativeBuildInputs = stacklockHaskellPkgSet: [
            final.cabal-install
            final.haskellPackages.fourmolu
            nixpkgs.legacyPackages.${final.system}.stack
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
