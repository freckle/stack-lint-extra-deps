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
  { stackageVersions :: Maybe StackageVersions
  , hackageVersions :: Maybe HackageVersions
  , gitDetails :: Maybe GitDetails
  }

getExternalDetails
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadHackage m
     , MonadStackage m
     , MonadGit m
     )
  => StackageResolver
  -> ExtraDep
  -> m ExternalDetails
getExternalDetails resolver = \case
  Hackage dep -> do
    ExternalDetails
      <$> getStackageVersions resolver dep.package
      <*> getHackageVersions dep.package
      <*> pure Nothing
  Git dep -> do
    let package = inferGitHackageName $ dep.repository
    ExternalDetails
      <$> getStackageVersions resolver package
      <*> getHackageVersions package
      <*> getGitDetails dep
  Other _ -> pure $ ExternalDetails Nothing Nothing Nothing

inferGitHackageName :: Repository -> PackageName
inferGitHackageName = PackageName . repositoryBaseName
