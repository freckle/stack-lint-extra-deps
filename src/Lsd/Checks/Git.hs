module Lsd.Checks.Git
  ( checkInGitClone
  ) where

import RIO

import Control.Error.Util (hoistMaybe)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Lsd.Cache
import Lsd.Check
import Lsd.ExtraDep
import Lsd.GitExtraDep
import Lsd.Suggestion
import RIO.FilePath (takeBaseName)
import RIO.Process
import RIO.Text (unpack)

checkInGitClone
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasLogFunc env
     , HasProcessContext env
     , HasCache env
     )
  => (GitExtraDep -> m (Maybe a))
  -> (a -> Maybe Suggestion)
  -> Check m
checkInGitClone setup suggest = Check $ \extraDep -> do
  runMaybeT $ do
    Git ged@GitExtraDep {..} <- pure extraDep

    let
      cloneUrl = unpack $ unRepository gedRepository
      cloneTo path = do
        logDebug $ "Cloning " <> fromString cloneUrl <> "..."
        proc "git" ["clone", "--quiet", cloneUrl, path] runProcess_

    a <- MaybeT $ withinCachedDirectory (takeBaseName cloneUrl) cloneTo $ do
      logDebug $ "Updating " <> fromString cloneUrl <> "..."
      proc "git" ["pull", "--quiet"] runProcess_
      setup ged

    hoistMaybe $ suggest a
