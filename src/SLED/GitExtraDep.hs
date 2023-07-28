module SLED.GitExtraDep
  ( GitExtraDep (..)
  , gitExtraDepToText
  , Repository (..)
  , repositoryBase
  , repositoryBaseName
  , CommitSHA (..)
  ) where

import SLED.Prelude

import Data.Aeson
import qualified Data.Text as T

data GitExtraDep = GitExtraDep
  { gedRepository :: Repository
  , gedCommit :: CommitSHA
  }
  deriving stock (Show)

instance FromJSON GitExtraDep where
  parseJSON = withObject "GitExtraDep" $ \o -> do
    gh <- o .:? "github"
    repo <- maybe (o .: "git") (pure . Repository . (ghBase <>)) gh
    GitExtraDep repo <$> o .: "commit"

gitExtraDepToText :: GitExtraDep -> Text
gitExtraDepToText GitExtraDep {..} =
  unRepository gedRepository
    <> "@"
    <> unCommitSHA gedCommit

newtype Repository = Repository
  { unRepository :: Text
  }
  deriving newtype (Show, FromJSON, ToJSON)

repositoryBase :: Repository -> Text
repositoryBase = dropPrefix ghBase . unRepository

repositoryBaseName :: Repository -> Text
repositoryBaseName = T.drop 1 . T.dropWhile (/= '/') . repositoryBase

newtype CommitSHA = CommitSHA
  { unCommitSHA :: Text
  }
  deriving newtype (Show, FromJSON)

instance ToJSON CommitSHA where
  toJSON (CommitSHA x) = toJSON $ T.take 7 x
  toEncoding (CommitSHA x) = toEncoding $ T.take 7 x

ghBase :: Text
ghBase = "https://github.com/"

dropPrefix :: Text -> Text -> Text
dropPrefix prefix t = fromMaybe t $ T.stripPrefix prefix t
