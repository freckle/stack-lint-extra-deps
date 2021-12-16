module Lsd.Hackage
  ( HackageDetails(..)
  , getHackageDetails
  ) where

import RIO

import Data.Aeson
import Lsd.PackageName
import Lsd.Version
import Network.HTTP.Simple
import Network.HTTP.Types.Header (hAccept)
import Network.HTTP.Types.Status (status200)
import qualified RIO.ByteString.Lazy as BSL
import RIO.List (headMaybe)
import RIO.Text (unpack)

newtype HackageDetails = HackageDetails
  { hdVersion :: Version
  }

getHackageDetails
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
  => PackageName
  -> m (Maybe HackageDetails)
getHackageDetails package = do
  req <-
    liftIO
    $ parseRequest
    $ unpack
    $ "https://hackage.haskell.org/package/"
    <> unPackageName package
    <> "/preferred"

  -- We'll parse ourselves because it makes logging more convenient
  resp <- httpLBS $ addRequestHeader hAccept "application/json" req

  let
    mBody = do
      guard $ getResponseStatus resp == status200
      pure $ getResponseBody resp
    mVersion = do
      body <- mBody
      HackageVersionsPreferred {..} <- decode body
      headMaybe hvpNormalVersion

  logDebug
    $ "Hackage details found for "
    <> display package
    <> ": "
    <> "\n  Status: "
    <> displayShow (getResponseStatus resp)
    <> "\n  Response: "
    <> maybe "none" (displayBytesUtf8 . BSL.toStrict) mBody
    <> "\n  Parsed version: "
    <> maybe "none" display mVersion

  pure $ HackageDetails <$> mVersion

-- | <https://hackage.haskell.org/api#versions>
data HackageVersionsPreferred = HackageVersionsPreferred
  { hvpNormalVersion :: [Version]
  , hvpUnpreferredVersion :: [Version]
  , hvpDeprecatedVersion :: [Version]
  }

instance FromJSON HackageVersionsPreferred where
  parseJSON = withObject "HackageVersionsPreferred" $ \o ->
    HackageVersionsPreferred
      <$> (o .:? "normal-version" .!= [])
      <*> (o .:? "unpreferred-version" .!= [])
      <*> (o .:? "deprecated-version" .!= [])
