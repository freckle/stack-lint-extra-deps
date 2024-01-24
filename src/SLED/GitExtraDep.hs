{-# LANGUAGE DuplicateRecordFields #-}

module SLED.GitExtraDep
  ( GitExtraDep (..)
  , decodeGitExtraDep
  , Repository (..)
  , repositoryBase
  , repositoryBaseName
  , CommitSHA (..)
  ) where

import SLED.Prelude

import qualified Data.Text as T
import Data.Yaml.Marked.Parse
import Data.Yaml.Marked.Value

data GitExtraDep = GitExtraDep
  { repository :: Repository
  , commit :: Marked CommitSHA
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

decodeGitExtraDep :: Marked Value -> Either String (Marked GitExtraDep)
decodeGitExtraDep = withObject "GitExtraDep" $ \o -> do
  gh <- o .:? "github"
  repo <-
    maybe
      (json =<< o .: "git")
      ((Repository . (ghBase <>) <$$>) . text)
      gh
  GitExtraDep (markedItem repo) <$> (json =<< o .: "commit")

newtype Repository = Repository
  { unwrap :: Text
  }
  deriving newtype (Eq, Show, FromJSON, ToJSON)

repositoryBase :: Repository -> Text
repositoryBase repo = dropPrefix ghBase repo.unwrap

repositoryBaseName :: Repository -> Text
repositoryBaseName = T.drop 1 . T.dropWhile (/= '/') . repositoryBase

newtype CommitSHA = CommitSHA
  { unwrap :: Text
  }
  deriving newtype (Eq, Show, FromJSON, ToJSON)

ghBase :: Text
ghBase = "https://github.com/"

dropPrefix :: Text -> Text -> Text
dropPrefix prefix t = fromMaybe t $ T.stripPrefix prefix t
