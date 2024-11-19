{-# LANGUAGE TemplateHaskell #-}

module SLED.StackageSpec
  ( spec
  ) where

import SLED.Prelude

import qualified Data.ByteString.Lazy as BSL
import Data.FileEmbed
import SLED.Stackage
import SLED.Test

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
            { onPage = unsafeVersion "2.1.0.0"
            , onHackage = unsafeVersion "2.2.2.0"
            }

    it "parses a page with revisions" $ do
      let body =
            BSL.fromStrict
              $(embedFile "test/files/stackage/lts-20.26/servant-swagger-ui-core-0.3.5.html")

      parseStackageVersions body
        `shouldBe` Right
          StackageVersions
            { onPage = unsafeVersion "0.3.5@rev:6"
            , onHackage = unsafeVersion "0.3.5@rev:11"
            }
