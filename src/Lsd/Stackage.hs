{-# LANGUAGE TupleSections #-}

module Lsd.Stackage
  ( StackageDetails(..)
  , getStackageDetails
  ) where

import RIO

import Data.Aeson
import Lsd.Cache
import Lsd.PackageName
import Lsd.StackageResolver
import Lsd.Version
import Network.HTTP.Simple
import Network.HTTP.Types.Status (status200)
import qualified RIO.ByteString.Lazy as BSL
import qualified RIO.Map as Map
import RIO.Text (unpack)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor

data StackageDetails = StackageDetails
  { sdStackageVersion :: Version
  , sdHackageVersion :: Version
  }

instance FromJSON StackageDetails where
  parseJSON = withObject "StackageDetails"
    $ \o -> StackageDetails <$> o .: "x" <*> o .: "y"

getStackageDetails
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasCache env)
  => StackageResolver
  -> PackageName
  -> m (Maybe StackageDetails)
getStackageDetails resolver package = do
  mHtml <- cachedFile cachePath $ getStackageHtml resolver package

  let
    mVersions = parseVersionsTable . fromDocument . parseLBS <$> mHtml
    showVersionPair (k, v) =
      "\n  " <> display k <> " => " <> fromString (showVersion v)

  logDebug
    $ "Stackage versions found for "
    <> display package
    <> ": "
    <> maybe "none" (mconcat . map showVersionPair . Map.toList) mVersions

  pure $ do
    versions <- mVersions
    StackageDetails
      <$> Map.lookup "Version on this page:" versions
      <*> Map.lookup "Latest on Hackage:" versions
 where
  cachePath =
    unpack $ unStackageResolver resolver <> "-" <> unPackageName package

getStackageHtml
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => StackageResolver
  -> PackageName
  -> m (Maybe BSL.ByteString)
getStackageHtml resolver package = do
  req <-
    liftIO
    $ parseRequest
    $ unpack
    $ "https://www.stackage.org/"
    <> unStackageResolver resolver
    <> "/package/"
    <> unPackageName package

  resp <- httpLBS req
  pure $ if getResponseStatus resp == status200
    then Just $ getResponseBody resp
    else Nothing

parseVersionsTable :: Cursor -> Map Text Version
parseVersionsTable cursor = do
  Map.fromList $ mapMaybe (toPair . ($// content)) $ cursor $// element "tr"
 where
  toPair = \case
    [] -> Nothing
    [_] -> Nothing
    [k, v] -> (k, ) <$> parseVersion (unpack v)
    [k, _, v] -> (k, ) <$> parseVersion (unpack v)
    (k : v : _) -> (k, ) <$> parseVersion (unpack v)
