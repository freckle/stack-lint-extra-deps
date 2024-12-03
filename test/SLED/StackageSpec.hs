{-# LANGUAGE TemplateHaskell #-}

module SLED.StackageSpec
  ( spec
  ) where

import SLED.Test

import qualified Data.ByteString.Lazy as BSL
import Data.FileEmbed
import SLED.Stackage

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
            { onPage = "2.1.0.0"
            , onHackage = "2.2.2.0"
            }

    it "parses a page with revisions" $ do
      let body =
            BSL.fromStrict
              $(embedFile "test/files/stackage/lts-20.26/servant-swagger-ui-core-0.3.5.html")

      parseStackageVersions body
        `shouldBe` Right
          StackageVersions
            { onPage = "0.3.5@rev:6"
            , onHackage = "0.3.5@rev:11"
            }
