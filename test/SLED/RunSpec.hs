module SLED.RunSpec
  ( spec
  ) where

import SLED.Prelude

import Blammo.Logging.Logger
import qualified Data.Map.Strict as Map
import SLED.Checks
import SLED.ExtraDep
import SLED.GitExtraDep
import SLED.Hackage
import SLED.HackageExtraDep
import SLED.PackageName
import SLED.Run
import SLED.StackYaml
import SLED.Stackage
import SLED.StackageResolver
import SLED.Suggestion
import SLED.Test
import SLED.Version

spec :: Spec
spec = do
  describe "shouldIncludeExtraDep" $ do
    context "Hackage extra deps" $ do
      it "is True by default" $ example $ do
        freckleApp1011 `shouldSatisfy` shouldIncludeExtraDep Nothing []

      it "is True if matching given include" $ example $ do
        freckleApp1011
          `shouldSatisfy` shouldIncludeExtraDep (Just "*-app") []

      it "is True if not matching any given excludes" $ example $ do
        freckleApp1011
          `shouldSatisfy` shouldIncludeExtraDep Nothing ["other-package"]

      it "is False if not matching a given include" $ example $ do
        freckleApp1011
          `shouldNotSatisfy` shouldIncludeExtraDep (Just "other-package") []

      it "is False is matching any given excludes" $ example $ do
        freckleApp1011
          `shouldNotSatisfy` shouldIncludeExtraDep Nothing ["freckle-*"]

    context "Git extra deps" $ do
      it "is True by default" $ example $ do
        yesodFlowRoutesGitHub `shouldSatisfy` shouldIncludeExtraDep Nothing []

      it "is True if org/repo matches given include" $ example $ do
        yesodFlowRoutesGitHub
          `shouldSatisfy` shouldIncludeExtraDep (Just "freckle/*") []

      it "is True if not matching any given excludes" $ example $ do
        yesodFlowRoutesGitHub
          `shouldSatisfy` shouldIncludeExtraDep Nothing ["other-package"]

      it "is False is not matching a given include" $ example $ do
        yesodFlowRoutesGitHub
          `shouldNotSatisfy` shouldIncludeExtraDep (Just "other-package") []

      it "is False is matching any given excludes" $ example $ do
        yesodFlowRoutesGitHub
          `shouldNotSatisfy` shouldIncludeExtraDep Nothing ["*/yesod-*"]

  describe "runChecks" $ do
    describe "HackageChecks" $ do
      describe "checkHackageVersion" $ do
        it "suggests newer normal versions" $ example $ do
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

        it "doesn't suggest deprecated versions" $ example $ do
          let mockHackage =
                Map.singleton (PackageName "freckle-app")
                  $ hackageVersions [] [] ["1.0.1.2"]

          suggestions <-
            runTestChecks mockHackage mempty lts1818 HackageChecks freckleApp1011

          suggestions `shouldBe` []

runTestChecks
  :: MonadUnliftIO m
  => Map PackageName HackageVersions
  -> Map StackageResolver (Map PackageName StackageVersions)
  -> StackageResolver
  -> ChecksName
  -> ExtraDep
  -> m [Suggestion]
runTestChecks mockHackage mockStackage resolver checksName extraDep = do
  testApp <-
    TestApp
      <$> newTestLogger defaultLogSettings
      <*> pure ignoredStackYaml
      <*> pure mockHackage
      <*> pure mockStackage

  runTestAppT (runChecks resolver checksName extraDep) testApp
 where
  -- TODO: stop mocking this, it's not useful anymore
  ignoredStackYaml =
    StackYaml
      { syResolver = StackageResolver "<ignored>"
      , syExtraDeps = []
      }

lts1818 :: StackageResolver
lts1818 = StackageResolver "lts-18.18"

freckleApp1011 :: ExtraDep
freckleApp1011 =
  Hackage
    HackageExtraDep
      { hedPackage = PackageName "freckle-app"
      , hedVersion = parseVersion "1.0.1.1"
      , hedChecksum = Nothing
      }

freckleApp1012 :: ExtraDep
freckleApp1012 =
  Hackage
    HackageExtraDep
      { hedPackage = PackageName "freckle-app"
      , hedVersion = parseVersion "1.0.1.2"
      , hedChecksum = Nothing
      }

yesodFlowRoutesGitHub :: ExtraDep
yesodFlowRoutesGitHub =
  Git
    GitExtraDep
      { gedRepository = Repository "https://github.com/freckle/yesod-routes-flow"
      , gedCommit = CommitSHA "2a9cd873880956dd9a0999b593022d3c746324e8"
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
