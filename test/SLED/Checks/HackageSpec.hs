module SLED.Checks.HackageSpec
  ( spec
  ) where

import SLED.Test

import SLED.Hackage (HackageVersions (..))

spec :: Spec
spec = do
  describe "checkHackageVersion" $ do
    it "suggests newer normal versions" $ do
      runTestAppM
        $ withHackage "freckle-app" "1.0.1.2"
        $ assertAutoFixed
          [ " resolver: lts-0.0"
          , " extra-deps:"
          , "-  - freckle-app-1.0.1.1"
          , "+  - freckle-app-1.0.1.2"
          ]

    it "doesn't suggest deprecated versions" $ do
      runTestAppM
        $ withHackageVersions
          "freckle-app"
          HackageVersions
            { normal = []
            , unpreferred = []
            , deprecated = ["1.0.1.2"]
            }
        $ assertNoFixes
          [ "resolver: lts-0.0"
          , "extra-deps:"
          , "  - freckle-app-1.0.1.1"
          ]

  describe "checkRedundantHackage" $ do
    it "suggests when stackage has your dep" $ do
      runTestAppM
        $ withStackage "lts-18.18" "freckle-app" "1.0.1.1"
        $ assertAutoFixed
          [ " resolver: lts-18.18"
          , " extra-deps:"
          , "-  - freckle-app-1.0.1.1"
          , "   - other-0.0.0" -- avoid leaving invalid extra-deps: null
          ]

    it "suggests when stackage has a newer dep" $ do
      runTestAppM
        $ withStackage "lts-18.18" "freckle-app" "1.0.1.2"
        $ assertAutoFixed
          [ " resolver: lts-18.18"
          , " extra-deps:"
          , "-  - freckle-app-1.0.1.1"
          , "   - other-0.0.0" -- avoid leaving invalid extra-deps: null
          ]

    it "does not suggest when stackage has an older dep" $ do
      runTestAppM
        $ withStackage "lts-18.18" "freckle-app" "1.0.1.0"
        $ assertNoFixes
          [ "resolver: lts-18.18"
          , "extra-deps:"
          , "  - freckle-app-1.0.1.1"
          ]

    it "does not suggest when the extra-dep is a newer revision (implicit)" $ do
      runTestAppM
        $ withHackage "servant-swagger-ui-core" "0.3.5@rev:11"
        $ withStackage "lts-18.18" "servant-swagger-ui-core" "0.3.5@rev:6"
        $ assertNoFixes
          [ "resolver: lts-18.18"
          , "extra-deps:"
          , "  - servant-swagger-ui-core-0.3.5"
          ]

    it "does not suggest when the extra-dep is a newer revision (checksum)" $ do
      runTestAppM
        $ withHackage "servant-swagger-ui-core" "0.3.5@rev:11"
        $ withStackage "lts-18.18" "servant-swagger-ui-core" "0.3.5@rev:6"
        $ assertNoFixes
          [ "resolver: lts-18.18"
          , "extra-deps:"
          , "  - servant-swagger-ui-core-0.3.5@sha256:ffffffff,100"
          ]

    it "does not suggest when the extra-dep is a newer revision (explicit)" $ do
      runTestAppM
        $ withHackage "servant-swagger-ui-core" "0.3.5@rev:11"
        $ withStackage "lts-18.18" "servant-swagger-ui-core" "0.3.5@rev:6"
        $ assertNoFixes
          [ "resolver: lts-18.18"
          , "extra-deps:"
          , "  - servant-swagger-ui-core-0.3.5@rev:11"
          ]
