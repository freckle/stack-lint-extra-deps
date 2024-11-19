module SLED.Version
  ( Version
  , defaultRevision
  , parseVersion
  , showVersion
  ) where

import SLED.Prelude

import Data.Aeson (withText)
import Data.Char (isDigit, isHexDigit)
import qualified Data.List.NonEmpty as NE
import qualified Data.Version as V
import Text.ParserCombinators.ReadP
  ( ReadP
  , char
  , eof
  , many1
  , readP_to_S
  , satisfy
  , string
  )
import qualified Prelude as Unsafe (read)

data Version = Version
  { version :: V.Version
  , revision :: Maybe Natural
  }
  deriving stock (Show, Eq, Ord)

instance ToJSON Version where
  toJSON v = toJSON $ "v" <> showVersion v
  toEncoding v = toEncoding $ "v" <> showVersion v

instance FromJSON Version where
  parseJSON =
    withText "Version"
      $ maybe (fail "Not a valid version") pure
      . parseVersion
      . unpack

defaultRevision :: [Version] -> Version -> Version
defaultRevision vs v =
  case find (eqOnVersion v) vs of
    Nothing -> v
    Just v' -> v {revision = v.revision <|> v'.revision}

eqOnVersion :: Version -> Version -> Bool
eqOnVersion = (==) `on` (.version)

parseVersion :: String -> Maybe Version
parseVersion =
  parse
    $ parseWithRevision
    <|> parseWithChecksum
    <|> parseSimple
    <* eof

parseWithRevision :: ReadP Version
parseWithRevision = do
  v <- V.parseVersion
  r <- string "@rev:" *> digits
  pure
    Version
      { version = v
      , revision = Just $ Unsafe.read r
      }
 where
  digits = many1 $ satisfy isDigit

parseWithChecksum :: ReadP Version
parseWithChecksum = do
  v <- V.parseVersion
  -- just discarded for now, but here if we want it later
  _checksum <- char '@' *> many1 hex
  pure
    Version
      { version = v
      , revision = Nothing
      }
 where
  hex = satisfy isHexDigit

parseSimple :: ReadP Version
parseSimple =
  Version
    <$> V.parseVersion
    <*> pure Nothing

showVersion :: Version -> String
showVersion (Version v mr) =
  V.showVersion v <> maybe "" (("@rev:" <>) . show) mr

parse :: ReadP a -> String -> Maybe a
parse p s = fst . NE.last <$> NE.nonEmpty (readP_to_S p s)
