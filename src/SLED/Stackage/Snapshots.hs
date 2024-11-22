module SLED.Stackage.Snapshots
  ( getLatestInSeries
  ) where

import SLED.Prelude

import Data.Aeson (withArray)
import Network.HTTP.Simple
import SLED.StackageResolver
import UnliftIO.Exception (try)

getLatestInSeries :: MonadUnliftIO m => StackageResolver -> m StackageResolver
getLatestInSeries x = maybe x (.resolver) <$> getFirstSnapshotMatching sameSeries
 where
  sameSeries :: Snapshot -> Bool
  sameSeries s = s.resolver `stackageResolverSameSeries` x

data Snapshot = Snapshot
  { resolver :: StackageResolver
  , _description :: Text
  , _since :: Text
  }

instance FromJSON Snapshot where
  parseJSON = withArray "Snapshot" $ \vs ->
    case toList vs of
      [resolver, description, since] ->
        Snapshot
          <$> parseJSON resolver
          <*> parseJSON description
          <*> parseJSON since
      _ -> fail $ "Snapshots are encoded as a list of 3 elements, saw " <> show vs

data SnapshotPage = SnapshotPage
  { snapshots :: [[Snapshot]]
  , totalCount :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

findSnapshotPage :: (Snapshot -> Bool) -> SnapshotPage -> Maybe Snapshot
findSnapshotPage p = find p . concat . (.snapshots)

snapshotPageLength :: SnapshotPage -> Int
snapshotPageLength = length . concat . (.snapshots)

getFirstSnapshotMatching
  :: MonadUnliftIO m => (Snapshot -> Bool) -> m (Maybe Snapshot)
getFirstSnapshotMatching p = go 0 1
 where
  go seenSoFar page = do
    result <- getSnapshotsPage page
    case result of
      Left _ex -> pure Nothing -- TODO: log it
      Right sp | ms@Just {} <- findSnapshotPage p sp -> pure ms
      Right sp | sp.totalCount > seenSoFar -> do
        go (seenSoFar + snapshotPageLength sp) (page + 1)
      Right {} -> pure Nothing

getSnapshotsPage
  :: MonadUnliftIO m => Int -> m (Either HttpException SnapshotPage)
getSnapshotsPage page = try $ do
  req <-
    liftIO
      $ parseRequestThrow
      $ "http://www.stackage.org/snapshots?page="
      <> show page
  resp <- httpJSON req
  pure $ getResponseBody resp
