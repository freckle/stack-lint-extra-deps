module Lsd.Hackage
  ( HackageDetails(..)
  , getHackageDetails
  ) where

import RIO

import Conduit
import Lsd.PackageName
import Lsd.Version
import Network.HTTP.Simple
import Network.HTTP.Types.Status (status200)
import RIO.List (headMaybe)
import RIO.Text (unpack)
import qualified RIO.Text as T
import Text.RSS.Conduit.Parse
import Text.RSS.Types
import Text.XML.Stream.Parse (def, parseLBS)

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
    <> ".rss"

  resp <- httpLBS req
  let
    mTitle = do
      guard $ getResponseStatus resp == status200

      doc <-
        join
        $ runConduit
        $ parseLBS def (getResponseBody resp)
        .| rssDocument @NoExtensions

      itemTitle <$> headMaybe (channelItems doc)

    mVersion = do
      title <- mTitle
      suffix <- T.stripPrefix (unPackageName package <> "-")
        $ T.takeWhile (/= ' ') title
      parseVersion $ unpack suffix

  logDebug
    $ "Hackage details found for "
    <> display package
    <> ": "
    <> "\n  RSS status: "
    <> displayShow (getResponseStatus resp)
    <> "\n  RSS item title: "
    <> maybe "none" display mTitle
    <> "\n  Parsed version: "
    <> maybe "none" display mVersion

  pure $ HackageDetails <$> mVersion
