module ExternalDetails
  ( ExternalDetails(..)
  , getExternalDetails
  ) where

import RIO

import ExtraDep
import GitDetails
import GitExtraDep
import Hackage
import HackageExtraDep
import PackageName
import Stackage
import StackageResolver
import RIO.Process

data ExternalDetails = ExternalDetails
  { edStackageVersions :: Maybe StackageVersions
  , edHackageVersions :: Maybe HackageVersions
  , edGitDetails :: Maybe GitDetails
  }

getExternalDetails
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
  => StackageResolver
  -> ExtraDep
  -> m ExternalDetails
getExternalDetails resolver = \case
  Hackage dep -> do
    let package = hedPackage dep
    ExternalDetails
      <$> getStackageVersions resolver package
      <*> getHackageVersions package
      <*> pure Nothing

  Git dep -> do
    let package = inferGitHackageName $ gedRepository dep
    ExternalDetails
      <$> getStackageVersions resolver package
      <*> getHackageVersions package
      <*> getGitDetails dep

  Other _ -> pure $ ExternalDetails Nothing Nothing Nothing

inferGitHackageName :: Repository -> PackageName
inferGitHackageName = packageName . repositoryBaseName
