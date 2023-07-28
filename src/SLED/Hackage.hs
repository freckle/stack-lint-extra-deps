module SLED.Hackage
  ( HackageVersions (..)
  , getHackageVersions
  ) where

import SLED.Prelude

import Data.Aeson
import Network.HTTP.Simple
import Network.HTTP.Types.Header (hAccept)
import Network.HTTP.Types.Status (Status (..), status200)
import SLED.PackageName
import SLED.Version

-- | <https://hackage.haskell.org/api#versions>
data HackageVersions = HackageVersions
  { hvNormal :: [Version]
  , hvUnpreferred :: [Version]
  , hvDeprecated :: [Version]
  }
  deriving stock (Show)

instance FromJSON HackageVersions where
  parseJSON = withObject "HackageVersions" $ \o ->
    HackageVersions
      <$> (o .:? "normal-version" .!= [])
      <*> (o .:? "unpreferred-version" .!= [])
      <*> (o .:? "deprecated-version" .!= [])

getHackageVersions
  :: (MonadUnliftIO m, MonadLogger m)
  => PackageName
  -> m (Maybe HackageVersions)
getHackageVersions package = do
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
    mVersions = do
      body <- mBody
      decode body

  logDebug
    $ "Hackage dependency details"
    :# [ "package" .= unPackageName package
       , "statusCode" .= statusCode (getResponseStatus resp)
       , "versions" .= (hvNormal <$> mVersions)
       ]

  pure mVersions
