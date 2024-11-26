module SLED.Version
  ( Version
  , defaultRevision
  , parseVersion
  , versionParser
  , showVersion
  ) where

import SLED.Prelude

import Data.Aeson (withText)
import qualified Data.Version as V
import SLED.Parse

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
    withText "Version" $ maybe (fail "Not a valid version") pure . parseVersion

defaultRevision :: [Version] -> Version -> Version
defaultRevision vs v =
  case find (eqOnVersion v) vs of
    Nothing -> v
    Just v' -> v {revision = v.revision <|> v'.revision}

eqOnVersion :: Version -> Version -> Bool
eqOnVersion = (==) `on` (.version)

parseVersion :: Text -> Maybe Version
parseVersion = parse versionParser

versionParser :: ReadP Version
versionParser =
  parseWithRevision
    <|> parseWithChecksum
    <|> parseSimple

parseWithRevision :: ReadP Version
parseWithRevision = do
  v <- V.parseVersion
  r <- string "@rev:" *> nat
  pure
    Version
      { version = v
      , revision = Just r
      }

parseWithChecksum :: ReadP Version
parseWithChecksum = do
  v <- V.parseVersion
  -- just discarded for now, but here if we want it later
  _checksum <- string "@sha256:" *> hexes <* char ',' <* nat
  pure
    Version
      { version = v
      , revision = Nothing
      }

parseSimple :: ReadP Version
parseSimple =
  Version
    <$> V.parseVersion
    <*> pure Nothing

showVersion :: Version -> String
showVersion (Version v mr) =
  V.showVersion v <> maybe "" (("@rev:" <>) . show) mr
