{-# LANGUAGE TemplateHaskell #-}

module SLED.StackageSpec
  ( spec
  ) where

import SLED.Prelude

import qualified Data.ByteString.Lazy as BSL
import Data.FileEmbed
import SLED.Stackage
import SLED.Version
import Test.Hspec

spec :: Spec
spec = do
  describe "parseStackageVersions" $ do
    it "parses an example page" $ do
      let body =
            BSL.fromStrict
              $(embedFile "test/files/stackage/lts-18.1/generic-lens-2.1.0.0.html")

      parseStackageVersions body
        `shouldBe` Right
          StackageVersions
            { svOnPage = unsafeVersion "2.1.0.0"
            , svOnHackage = unsafeVersion "2.2.2.0"
            }

unsafeVersion :: String -> Version
unsafeVersion s = fromMaybe err $ parseVersion s
 where
  err = error $ pack $ "Not a valid version: " <> s
