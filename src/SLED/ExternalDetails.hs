module SLED.ExternalDetails
  ( ExternalDetails (..)
  , getExternalDetails
  ) where

import SLED.Prelude

import SLED.ExtraDep
import SLED.GitDetails
import SLED.GitExtraDep
import SLED.Hackage
import SLED.HackageExtraDep
import SLED.PackageName
import SLED.Stackage
import SLED.StackageResolver

data ExternalDetails = ExternalDetails
  { edStackageVersions :: Maybe StackageVersions
  , edHackageVersions :: Maybe HackageVersions
  , edGitDetails :: Maybe GitDetails
  }

getExternalDetails
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
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
