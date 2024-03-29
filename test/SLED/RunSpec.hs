module SLED.RunSpec
  ( spec
  ) where

import SLED.Prelude

import SLED.Run
import SLED.Test
import SLED.Version

spec :: Spec
spec = do
  describe "shouldIncludeExtraDep" $ do
    context "Hackage extra deps" $ do
      it "is True by default" $ do
        Hackage freckleApp1011
          `shouldSatisfy` shouldIncludeExtraDep Nothing []

      it "is True if matching given include" $ do
        Hackage freckleApp1011
          `shouldSatisfy` shouldIncludeExtraDep (Just "*-app") []

      it "is True if not matching any given excludes" $ do
        Hackage freckleApp1011
          `shouldSatisfy` shouldIncludeExtraDep Nothing ["other-package"]

      it "is False if not matching a given include" $ do
        Hackage freckleApp1011
          `shouldNotSatisfy` shouldIncludeExtraDep (Just "other-package") []

      it "is False is matching any given excludes" $ do
        Hackage freckleApp1011
          `shouldNotSatisfy` shouldIncludeExtraDep Nothing ["freckle-*"]

    context "Git extra deps" $ do
      it "is True by default" $ do
        Git yesodFlowRoutesGitHub
          `shouldSatisfy` shouldIncludeExtraDep Nothing []

      it "is True if org/repo matches given include" $ do
        Git yesodFlowRoutesGitHub
          `shouldSatisfy` shouldIncludeExtraDep (Just "freckle/*") []

      it "is True if not matching any given excludes" $ do
        Git yesodFlowRoutesGitHub
          `shouldSatisfy` shouldIncludeExtraDep Nothing ["other-package"]

      it "is False is not matching a given include" $ do
        Git yesodFlowRoutesGitHub
          `shouldNotSatisfy` shouldIncludeExtraDep (Just "other-package") []

      it "is False is matching any given excludes" $ do
        Git yesodFlowRoutesGitHub
          `shouldNotSatisfy` shouldIncludeExtraDep Nothing ["*/yesod-*"]

freckleApp1011 :: HackageExtraDep
freckleApp1011 =
  HackageExtraDep
    { package = PackageName "freckle-app"
    , version = parseVersion "1.0.1.1"
    , checksum = Nothing
    }

yesodFlowRoutesGitHub :: GitExtraDep
yesodFlowRoutesGitHub =
  GitExtraDep
    { repository = Repository "https://github.com/freckle/yesod-routes-flow"
    , commit = markAtZero $ CommitSHA "2a9cd873880956dd9a0999b593022d3c746324e8"
    }
