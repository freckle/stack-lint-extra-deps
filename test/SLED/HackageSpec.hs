{-# LANGUAGE TemplateHaskell #-}

module SLED.HackageSpec
  ( spec
  )
where

import SLED.Prelude

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.FileEmbed
import SLED.Hackage
import SLED.Version
import Test.Hspec

spec :: Spec
spec = do
  describe "FromJSON" $ do
    it "parses a Hackage JSON response" $ do
      let body =
            BSL.fromStrict $(embedFile "test/files/hackage/freckle-app.json")

      eitherDecode body
        `shouldBe` Right
          HackageVersions
            { normal =
                catMaybes
                  [ parseVersion "1.9.1.1"
                  , parseVersion "1.9.1.0"
                  , parseVersion "1.9.0.3"
                  , parseVersion "1.9.0.2"
                  , parseVersion "1.9.0.1"
                  , parseVersion "1.9.0.0"
                  , parseVersion "1.8.1.0"
                  , parseVersion "1.8.0.0"
                  , parseVersion "1.7.1.0"
                  , parseVersion "1.7.0.0"
                  , parseVersion "1.6.0.3"
                  , parseVersion "1.6.0.2"
                  , parseVersion "1.6.0.1"
                  , parseVersion "1.6.0.0"
                  , parseVersion "1.5.1.0"
                  , parseVersion "1.5.0.1"
                  , parseVersion "1.5.0.0"
                  , parseVersion "1.4.0.0"
                  , parseVersion "1.3.0.0"
                  , parseVersion "1.2.0.2"
                  , parseVersion "1.2.0.1"
                  , parseVersion "1.2.0.0"
                  , parseVersion "1.1.0.0"
                  , parseVersion "1.0.4.0"
                  , parseVersion "1.0.3.0"
                  , parseVersion "1.0.2.10"
                  , parseVersion "1.0.2.9"
                  , parseVersion "1.0.2.8"
                  , parseVersion "1.0.2.7"
                  , parseVersion "1.0.2.6"
                  , parseVersion "1.0.2.5"
                  , parseVersion "1.0.2.4"
                  , parseVersion "1.0.2.3"
                  , parseVersion "1.0.2.2"
                  , parseVersion "1.0.2.1"
                  , parseVersion "1.0.2.0"
                  , parseVersion "1.0.1.0"
                  , parseVersion "1.0.0.4"
                  , parseVersion "1.0.0.3"
                  , parseVersion "1.0.0.2"
                  , parseVersion "1.0.0.1"
                  ]
            , unpreferred = []
            , deprecated = []
            }
