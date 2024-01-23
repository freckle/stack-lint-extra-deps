module SLED.Checks.HackageSpec
  ( spec
  ) where

import SLED.Prelude

import qualified Data.Map.Strict as Map
import SLED.Checks
import SLED.Hackage
import SLED.PackageName
import SLED.Stackage
import SLED.Suggestion
import SLED.Test
import SLED.Version

spec :: Spec
spec = do
  describe "checkHackageVersion" $ do
    it "suggests newer normal versions" $ do
      let
        mockHackage =
          Map.singleton (PackageName "freckle-app")
            $ hackageVersions ["1.0.1.2"] [] []
        extraDep = markAtZero (Hackage freckleApp1011) "<input>"

      suggestions <-
        runTestChecks
          mockHackage
          mempty
          Nothing
          lts1818
          HackageChecks
          extraDep

      suggestions
        `shouldBe` [ Suggestion
                      { target = extraDep
                      , action =
                          replaceHackageDep (freckleApp1011 <$ extraDep) $ unsafeVersion "1.0.1.2"
                      , description = "Newer version is available"
                      }
                   ]

    it "doesn't suggest deprecated versions" $ do
      let
        mockHackage =
          Map.singleton (PackageName "freckle-app")
            $ hackageVersions [] [] ["1.0.1.2"]
        extraDep = markAtZero (Hackage freckleApp1011) "<input>"

      suggestions <-
        runTestChecks
          mockHackage
          mempty
          Nothing
          lts1818
          HackageChecks
          extraDep

      suggestions `shouldBe` []

  describe "checkRedundantHackage" $ do
    it "suggests when stackage has your dep" $ do
      let
        mockStackage =
          Map.singleton
            (markedItem lts1818)
            ( Map.singleton (PackageName "freckle-app")
                $ stackageVersions "1.0.1.1" "1.0.1.2"
            )
        extraDep = markAtZero (Hackage freckleApp1011) "<input>"

      suggestions <-
        runTestChecks
          mempty
          mockStackage
          Nothing
          lts1818
          HackageChecks
          extraDep

      suggestions
        `shouldBe` [ Suggestion
                      { target = extraDep
                      , action = Remove
                      , description = "Same or newer version is now in your resolver"
                      }
                   ]

    it "suggests when stackage has a newer dep" $ do
      let
        mockStackage =
          Map.singleton
            (markedItem lts1818)
            ( Map.singleton (PackageName "freckle-app")
                $ stackageVersions "1.0.1.2" "1.0.1.2"
            )
        extraDep = markAtZero (Hackage freckleApp1011) "<input>"

      suggestions <-
        runTestChecks
          mempty
          mockStackage
          Nothing
          lts1818
          HackageChecks
          extraDep

      suggestions
        `shouldBe` [ Suggestion
                      { target = extraDep
                      , action = Remove
                      , description = "Same or newer version is now in your resolver"
                      }
                   ]

    it "does not suggest when stackage has an older dep" $ do
      let
        mockStackage =
          Map.singleton
            (markedItem lts1818)
            ( Map.singleton (PackageName "freckle-app")
                $ stackageVersions "1.0.1.0" "1.0.1.2"
            )
        extraDep = markAtZero (Hackage freckleApp1011) "<input>"

      suggestions <-
        runTestChecks
          mempty
          mockStackage
          Nothing
          lts1818
          HackageChecks
          extraDep

      suggestions `shouldBe` []

hackageVersions
  :: [String]
  -- ^ Normal
  -> [String]
  -- ^ Unpreferred
  -> [String]
  -- ^ Deprecated
  -> HackageVersions
hackageVersions n u d =
  HackageVersions
    { normal = mapMaybe parseVersion n
    , unpreferred = mapMaybe parseVersion u
    , deprecated = mapMaybe parseVersion d
    }

stackageVersions
  :: String
  -- ^ On-page
  -> String
  -- ^ On-Hackage
  -> StackageVersions
stackageVersions p h =
  StackageVersions
    { onPage = unsafeVersion p
    , onHackage = unsafeVersion h
    }
