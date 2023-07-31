{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module SLED.Stackage
  ( MonadStackage (..)
  , StackageVersions (..)
  , parseStackageVersions
  ) where

import SLED.Prelude

import Control.Error.Util (note)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import SLED.PackageName
import SLED.StackageResolver
import SLED.Version
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor

class MonadStackage m where
  getStackageVersions
    :: StackageResolver -> PackageName -> m (Maybe StackageVersions)

data StackageVersions = StackageVersions
  { svOnPage :: Version
  , svOnHackage :: Version
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

parseStackageVersions :: BSL.ByteString -> Either Text StackageVersions
parseStackageVersions bs =
  StackageVersions
    <$> note "Missing version on this page" (Map.lookup currentKey versions)
    <*> note "Missing latest on hackage" (Map.lookup latestKey versions)
 where
  versions = parseVersionsTable $ fromDocument $ parseLBS bs

parseVersionsTable :: Cursor -> Map Text Version
parseVersionsTable cursor = do
  fixNightly
    $ Map.fromList
    $ mapMaybe (toPair . ($// content))
    $ cursor
    $// element "tr"
 where
  toPair = \case
    [] -> Nothing
    [_] -> Nothing
    [k, v] -> (k,) <$> parseVersion (unpack v)
    [k, _, v] -> (k,) <$> parseVersion (unpack v)
    (k : v : _) -> (k,) <$> parseVersion (unpack v)

  fixNightly m =
    maybe m (\(_, v) -> Map.insertWith (\_new old -> old) currentKey v m)
      $ find ((nightlyPrefix `T.isPrefixOf`) . fst)
      $ Map.toList m

currentKey :: Text
currentKey = "Version on this page:"

latestKey :: Text
latestKey = "Latest on Hackage:"

nightlyPrefix :: Text
nightlyPrefix = "Stackage Nightly "
