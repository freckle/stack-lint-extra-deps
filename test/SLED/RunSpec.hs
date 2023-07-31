module SLED.RunSpec
  ( spec
  ) where

import SLED.Prelude

import SLED.Run
import SLED.Test

spec :: Spec
spec = do
  describe "shouldIncludeExtraDep" $ do
    context "Hackage extra deps" $ do
      it "is True by default" $ do
        freckleApp1011 `shouldSatisfy` shouldIncludeExtraDep Nothing []

      it "is True if matching given include" $ do
        freckleApp1011
          `shouldSatisfy` shouldIncludeExtraDep (Just "*-app") []

      it "is True if not matching any given excludes" $ do
        freckleApp1011
          `shouldSatisfy` shouldIncludeExtraDep Nothing ["other-package"]

      it "is False if not matching a given include" $ do
        freckleApp1011
          `shouldNotSatisfy` shouldIncludeExtraDep (Just "other-package") []

      it "is False is matching any given excludes" $ do
        freckleApp1011
          `shouldNotSatisfy` shouldIncludeExtraDep Nothing ["freckle-*"]

    context "Git extra deps" $ do
      it "is True by default" $ do
        yesodFlowRoutesGitHub `shouldSatisfy` shouldIncludeExtraDep Nothing []

      it "is True if org/repo matches given include" $ do
        yesodFlowRoutesGitHub
          `shouldSatisfy` shouldIncludeExtraDep (Just "freckle/*") []

      it "is True if not matching any given excludes" $ do
        yesodFlowRoutesGitHub
          `shouldSatisfy` shouldIncludeExtraDep Nothing ["other-package"]

      it "is False is not matching a given include" $ do
        yesodFlowRoutesGitHub
          `shouldNotSatisfy` shouldIncludeExtraDep (Just "other-package") []

      it "is False is matching any given excludes" $ do
        yesodFlowRoutesGitHub
          `shouldNotSatisfy` shouldIncludeExtraDep Nothing ["*/yesod-*"]
