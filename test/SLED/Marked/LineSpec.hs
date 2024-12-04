module SLED.Marked.LineSpec
  ( spec
  ) where

import SLED.Test

import SLED.Marked.Line

spec :: Spec
spec = do
  let
    exampleBS :: ByteString
    exampleBS = "this is line one\nthis is line two\nthis is line 3\n"
  --             01234567890123456 78901234567890123 456789012345678
  --                       1111111 11122222222223333 333333444444444
  --             |---------------| |---------------| |-------------|
  --             0              16 17             33 34           48

  describe "startOfStartLine" $ do
    it "finds the start of the start line of a mark" $ do
      startOfStartLine exampleBS (markAt 22 38) `shouldBe` 17

    it "works with content at the start of a line" $ do
      startOfStartLine exampleBS (markAt 17 22) `shouldBe` 17

    it "works with content at the start overall" $ do
      startOfStartLine exampleBS (markAt 0 17) `shouldBe` 0

    it "works with empty content" $ do
      startOfStartLine "" (markAt 0 17) `shouldBe` 0

  describe "endOfEndLine" $ do
    it "finds the end of the end line of a mark" $ do
      endOfEndLine exampleBS (markAt 22 38) `shouldBe` 48

    it "works with content at the end of a line" $ do
      endOfEndLine exampleBS (markAt 22 33) `shouldBe` 33

    it "works with content at the end overall" $ do
      endOfEndLine exampleBS (markAt 38 48) `shouldBe` 48

    it "works with content outside the end" $ do
      endOfEndLine exampleBS (markAt 38 49) `shouldBe` 48

    it "works with empty content" $ do
      endOfEndLine "" (markAt 38 49) `shouldBe` 0

markAt :: Natural -> Natural -> Marked ()
markAt s e =
  Marked
    { markedItem = ()
    , markedPath = "unused"
    , markedJSONPath = Nothing
    , markedLocationStart = Location s 0 0 -- line/column unused
    , markedLocationEnd = Location e 0 0 -- line/column unused
    }
