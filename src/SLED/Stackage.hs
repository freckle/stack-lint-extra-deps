{-# LANGUAGE TupleSections #-}

module SLED.Stackage
  ( StackageVersions (..)
  , getStackageVersions
  ) where

import RIO

import Data.List (find)
import Network.HTTP.Simple
import Network.HTTP.Types.Status (status200)
import qualified RIO.Map as Map
import RIO.Text (unpack)
import qualified RIO.Text as T
import SLED.PackageName
import SLED.StackageResolver
import SLED.Version
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor

data StackageVersions = StackageVersions
  { svOnPage :: Version
  , svOnHackage :: Version
  }

getStackageVersions
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
  => StackageResolver
  -> PackageName
  -> m (Maybe StackageVersions)
getStackageVersions resolver package = do
  req <-
    liftIO
      $ parseRequest
      $ unpack
      $ "https://www.stackage.org/"
      <> unStackageResolver resolver
      <> "/package/"
      <> unPackageName package

  resp <- httpLBS req

  let
    mBody = do
      guard $ getResponseStatus resp == status200
      pure $ getResponseBody resp
    mVersions = parseVersionsTable . fromDocument . parseLBS <$> mBody

  logDebug
    $ "Stackage details for "
    <> display package
    <> ": "
    <> "\n  Status: "
    <> displayShow (getResponseStatus resp)
    <> "\n  Versions: "
    <> maybe "none" displayVersions mVersions

  pure $ do
    versions <- mVersions
    StackageVersions
      <$> Map.lookup currentKey versions
      <*> Map.lookup latestKey versions

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

displayVersions :: Map Text Version -> Utf8Builder
displayVersions = mconcat . map displayPair . Map.toList
 where
  displayPair (k, v) =
    "\n  " <> display k <> " => " <> fromString (showVersion v)
