module SLED.RunSpec
  ( spec
  ) where

import SLED.Prelude

import Blammo.Logging.Logger
import SLED.Checks
import SLED.ExtraDep
import SLED.Hackage
import SLED.HackageExtraDep
import SLED.PackageName
import SLED.Run
import SLED.StackYaml
import SLED.StackageResolver
import SLED.Test
import SLED.Version

spec :: Spec
spec = do
  describe "runLsd" $ do
    describe "HackageChecks" $ do
      describe "checkHackageVersion" $ do
        it "suggests newer normal versions" $ example $ do
          testApp <-
            TestApp
              <$> newTestLogger defaultLogSettings
              <*> pure
                StackYaml
                  { syResolver = StackageResolver "lts-18.18"
                  , syExtraDeps =
                      [ freckleApp1011
                      ]
                  }
              <*> pure
                ( \case
                    PackageName "freckle-app" ->
                      Just
                        $ hackageVersions
                          ["1.0.1.2"]
                          []
                          []
                    _ -> Nothing
                )
              <*> pure (\_ -> const Nothing)

          flip runTestAppT testApp $ do
            runLsd "<ignored>" Nothing HackageChecks Nothing [] `shouldReturn` 1

        it "doesn't suggest deprecated versions" $ example $ do
          testApp <-
            TestApp
              <$> newTestLogger defaultLogSettings
              <*> pure
                StackYaml
                  { syResolver = StackageResolver "lts-18.18"
                  , syExtraDeps =
                      [ freckleApp1011
                      ]
                  }
              <*> pure
                ( \case
                    PackageName "freckle-app" ->
                      Just $ hackageVersions [] [] ["1.0.1.2"]
                    _ -> Nothing
                )
              <*> pure (\_ -> const Nothing)

          flip runTestAppT testApp $ do
            runLsd "<ignored>" Nothing HackageChecks Nothing [] `shouldReturn` 0

freckleApp1011 :: ExtraDep
freckleApp1011 =
  Hackage
    HackageExtraDep
      { hedPackage = PackageName "freckle-app"
      , hedVersion = parseVersion "1.0.1.1"
      , hedChecksum = Nothing
      }

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
