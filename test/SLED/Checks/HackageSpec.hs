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
      let mockHackage =
            Map.singleton (PackageName "freckle-app")
              $ hackageVersions ["1.0.1.2"] [] []

      suggestions <-
        runTestChecks mockHackage mempty lts1818 HackageChecks freckleApp1011

      suggestions
        `shouldBe` [ Suggestion
                      { sTarget = freckleApp1011
                      , sAction = ReplaceWith freckleApp1012
                      , sDescription = "Newer version is available"
                      }
                   ]

    it "doesn't suggest deprecated versions" $ do
      let mockHackage =
            Map.singleton (PackageName "freckle-app")
              $ hackageVersions [] [] ["1.0.1.2"]

      suggestions <-
        runTestChecks mockHackage mempty lts1818 HackageChecks freckleApp1011

      suggestions `shouldBe` []

  describe "checkRedundantHackage" $ do
    it "suggests when stackage has your dep" $ do
      let mockStackage =
            Map.singleton
              lts1818
              ( Map.singleton (PackageName "freckle-app")
                  $ stackageVersions "1.0.1.1" "1.0.1.2"
              )

      suggestions <-
        runTestChecks mempty mockStackage lts1818 HackageChecks freckleApp1011

      suggestions
        `shouldBe` [ Suggestion
                      { sTarget = freckleApp1011
                      , sAction = Remove
                      , sDescription = "Same or newer version is now in your resolver"
                      }
                   ]

    it "suggests when stackage has a newer dep" $ do
      let mockStackage =
            Map.singleton
              lts1818
              ( Map.singleton (PackageName "freckle-app")
                  $ stackageVersions "1.0.1.2" "1.0.1.2"
              )

      suggestions <-
        runTestChecks mempty mockStackage lts1818 HackageChecks freckleApp1011

      suggestions
        `shouldBe` [ Suggestion
                      { sTarget = freckleApp1011
                      , sAction = Remove
                      , sDescription = "Same or newer version is now in your resolver"
                      }
                   ]

    it "does not suggest when stackage has an older dep" $ do
      let mockStackage =
            Map.singleton
              lts1818
              ( Map.singleton (PackageName "freckle-app")
                  $ stackageVersions "1.0.1.0" "1.0.1.2"
              )

      suggestions <-
        runTestChecks mempty mockStackage lts1818 HackageChecks freckleApp1011

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
    { hvNormal = mapMaybe parseVersion n
    , hvUnpreferred = mapMaybe parseVersion u
    , hvDeprecated = mapMaybe parseVersion d
    }

stackageVersions
  :: String
  -- ^ On-page
  -> String
  -- ^ On-Hackage
  -> StackageVersions
stackageVersions p h =
  StackageVersions
    { svOnPage = unsafeVersion p
    , svOnHackage = unsafeVersion h
    }

unsafeVersion :: String -> Version
unsafeVersion s = fromMaybe err $ parseVersion s
 where
  err = error $ pack $ "Invalid version: " <> s
