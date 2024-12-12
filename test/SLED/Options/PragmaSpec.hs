module SLED.Options.PragmaSpec
  ( spec
  ) where

import SLED.Test

import qualified Data.ByteString.Char8 as BS
import qualified Data.List as List
import SLED.Options (Options (..))
import SLED.Options.Pragma

spec :: Spec
spec = do
  describe "parsePragmaOptions" $ do
    it "parses simple comments" $ do
      let
        yaml :: ByteString
        yaml =
          BS.unlines
            [ "# @sled --exclude foo --exclude bar"
            , "resolver: x"
            , "extra-deps:"
            , "# This is another comment that might mention"
            , "# sled at the beginning of a sentence."
            , "- foo"
            , "- bar"
            , "- baz"
            ]

      let (_errs, options) = parsePragmaOptions yaml

      options.excludes `shouldBe` ["foo", "bar"]

    it "parses multiple comments anywhere in the file" $ do
      let
        yaml :: ByteString
        yaml =
          BS.unlines
            [ "# @sled --exclude foo --exclude baz"
            , "resolver: x"
            , "extra-deps:"
            , "  - foo"
            , "  - bar"
            , "  # @sled --exclude=\"bat\""
            , "  - bat"
            ]

      let (_errs, options) = parsePragmaOptions yaml

      options.excludes `shouldBe` ["foo", "baz", "bat"]

    it "parses trailing comments" $ do
      let
        yaml :: ByteString
        yaml =
          BS.unlines
            [ "resolver: x"
            , "extra-deps:"
            , "  - foo"
            , "  - bar"
            , "  - bat # @sled --exclude=\"bat\""
            ]

      let (_errs, options) = parsePragmaOptions yaml

      options.excludes `shouldBe` ["bat"]

    it "returns errors in the case of invalid options" $ do
      let
        yaml :: ByteString
        yaml =
          BS.unlines
            [ "# @sled --what --no-way"
            , "resolver: x"
            ]

      let
        (errs, _) = parsePragmaOptions yaml

        expectedErr =
          intercalate "\n"
            [ "Invalid option `--what'"
            , ""
            , "Usage: stack-lint-extra-deps [-p|--path PATH] [-r|--resolver RESOLVER] "
            , "                             [-f|--format tty|gha|json] [--exclude PATTERN] "
            , "                             [-R|--no-check-resolver] [--checks CHECKS] "
            , "                             [-n|--no-exit] [-F|--fix] [PATTERN] [--version]"
            , ""
            , "  stack lint-extra-deps (sled)"
            ]

      errs `shouldBe` [expectedErr]

    it "returns errors in the case of invalid pragmas" $ do
      let
        yaml :: ByteString
        yaml =
          BS.unlines
            [ "# @sled foo '"
            , "resolver: x"
            ]

      let
        (errs, _) = parsePragmaOptions yaml

        expectedErr =
          List.unlines
            [ "<input>:1:6:"
            , "  |"
            , "1 | foo '"
            , "  |      ^"
            , "unexpected end of input"
            , "expecting ''', escaped'\\'', or escaped'\\\\'"
            ]

      errs `shouldBe` [expectedErr]

    it "collects all errors" $ do
      let
        yaml :: ByteString
        yaml =
          BS.unlines
            [ "# @sled foo '"
            , "resolver: x"
            , "# @sled bar '"
            ]

      let (errs, _) = parsePragmaOptions yaml

      errs `shouldSatisfy` (== 2) . length
