module GitExtraDep
  ( GitExtraDep(..)
  , Repository(..)
  , repositoryBase
  , repositoryBaseName
  , CommitSHA(..)
  ) where

import RIO

import Data.Aeson
import qualified RIO.Text as T

data GitExtraDep = GitExtraDep
  { gedRepository :: Repository
  , gedCommit :: CommitSHA
  }
  deriving stock Show

instance FromJSON GitExtraDep where
  parseJSON = withObject "GitExtraDep" $ \o -> do
    gh <- o .:? "github"
    repo <- maybe (o .: "git") (pure . Repository . (ghBase <>)) gh
    GitExtraDep repo <$> o .: "commit"

instance Display GitExtraDep where
  display GitExtraDep {..} = display gedRepository <> "@" <> display gedCommit

newtype Repository = Repository
    { unRepository :: Text
    }
    deriving newtype (Show, Display, FromJSON)

repositoryBase :: Repository -> Text
repositoryBase = T.dropPrefix ghBase . unRepository

repositoryBaseName :: Repository -> Text
repositoryBaseName = T.drop 1 . T.dropWhile (/= '/') . repositoryBase

newtype CommitSHA = CommitSHA
    { unCommitSHA :: Text
    }
    deriving newtype (Show, FromJSON)

instance Display CommitSHA where
  display (CommitSHA x) = display $ T.take 7 x

ghBase :: Text
ghBase = "https://github.com/"
