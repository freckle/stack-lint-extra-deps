module SLED.Options.Pragma
  ( parsePragmaOptions
  ) where

import SLED.Prelude

import qualified Data.ByteString.Char8 as BS8
import Data.Char (isSpace)
import qualified Data.Text as T
import Options.Applicative (ParserInfo, ParserResult (..))
import qualified Options.Applicative as Options
import SLED.Options
import qualified ShellWords

parsePragmaOptions :: ByteString -> ([String], Options)
parsePragmaOptions =
  second fold . partitionEithers . mapMaybe parsePragma . BS8.lines

parsePragma :: ByteString -> Maybe (Either String Options)
parsePragma =
  fmap (execParser optionsParserInfo <=< ShellWords.parse) . fromComment

fromComment :: ByteString -> Maybe String
fromComment bs =
  unpack <$> do
    c <-
      T.stripPrefix "# "
        . T.dropWhile (/= '#')
        $ decodeUtf8With lenientDecode bs
    T.stripPrefix "@sled " $ T.dropWhile isSpace c

execParser :: ParserInfo a -> [String] -> Either String a
execParser p = handleParserResult . Options.execParserPure Options.defaultPrefs p

handleParserResult :: ParserResult a -> Either String a
handleParserResult = \case
  Success a -> Right a
  Failure x -> Left $ fst $ Options.renderFailure x "stack-lint-extra-deps"
  CompletionInvoked {} -> Left "Unexpected completion invocation"
