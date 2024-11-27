module SLED.HackageExtraDepSpec
  ( spec
  ) where

import SLED.Prelude

import Data.Aeson.Types (JSONPathElement (..))
import Data.Yaml.Marked.Decode
import Data.Yaml.Marked.Parse
import SLED.HackageExtraDep
import SLED.Test

spec :: Spec
spec = do
  describe "decodeHackageExtraDep" $ do
    describe "successful decoding" $ do
      it "simple" $ do
        mdeps <- tryDecode "- foo-0.1.0.0\n"
        --                        ^^^^^^^^
        --                  01234567890123
        --                            1111
        map markedItem (markedItem mdeps)
          `shouldBe` [ HackageExtraDep
                        { package = PackageName "foo"
                        , version =
                            Marked
                              { markedItem = unsafeVersion "0.1.0.0"
                              , markedPath = "<input>"
                              , markedJSONPath = Just [Index 0]
                              , markedLocationStart = Location 6 0 6
                              , markedLocationEnd = Location 13 0 13
                              }
                        }
                     ]

      it "multi-hyphenated" $ do
        mdeps <- tryDecode "- optparse-applicative-0.1.0.0\n"
        --                                         ^^^^^^^^
        --                  0123456789012345678901234567890
        --                            111111111122222222223
        map markedItem (markedItem mdeps)
          `shouldBe` [ HackageExtraDep
                        { package = PackageName "optparse-applicative"
                        , version =
                            Marked
                              { markedItem = unsafeVersion "0.1.0.0"
                              , markedPath = "<input>"
                              , markedJSONPath = Just [Index 0]
                              , markedLocationStart = Location 23 0 23
                              , markedLocationEnd = Location 30 0 30
                              }
                        }
                     ]

      it "with revision" $ do
        mdeps <- tryDecode "- foo-0.1.0.0@rev:5\n"
        --                        ^^^^^^^^^^^^^^
        --                  01234567890123456789
        --                            1111111111
        map markedItem (markedItem mdeps)
          `shouldBe` [ HackageExtraDep
                        { package = PackageName "foo"
                        , version =
                            Marked
                              { markedItem = unsafeVersion "0.1.0.0@rev:5"
                              , markedPath = "<input>"
                              , markedJSONPath = Just [Index 0]
                              , markedLocationStart = Location 6 0 6
                              , markedLocationEnd = Location 19 0 19
                              }
                        }
                     ]

      it "with revision" $ do
        mdeps <- tryDecode "- foo-0.1.0.0@sha256:ffffff,100\n"
        --                        ^^^^^^^^^^^^^^^^^^^^^^^^^^
        --                  01234567890123456789012345678901
        --                            1111111111222222222233
        map markedItem (markedItem mdeps)
          `shouldBe` [ HackageExtraDep
                        { package = PackageName "foo"
                        , version =
                            Marked
                              { markedItem = unsafeVersion "0.1.0.0@sha256:ffffff,100"
                              , markedPath = "<input>"
                              , markedJSONPath = Just [Index 0]
                              , markedLocationStart = Location 6 0 6
                              , markedLocationEnd = Location 31 0 31
                              }
                        }
                     ]

tryDecode :: ByteString -> IO (Marked [Marked HackageExtraDep])
tryDecode = decodeThrow (array decodeHackageExtraDep) "<input>"
