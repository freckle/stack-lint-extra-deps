module Lsd.Run
  ( runLsd
  ) where

import RIO

import Lsd.Check
import Lsd.Checks
import Lsd.Options
import Lsd.StackYaml
import RIO.Process
import System.FilePath.Glob

runLsd
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
  => Options
  -> StackYaml
  -> (ExtraDep -> Suggestion -> m ())
  -> m Int
runLsd Options {..} StackYaml {..} report = do
  results <- for extraDeps $ \extraDep -> do
    details <- getExternalDetails resolver extraDep

    for checks $ \check -> do
      let mSuggestion = runCheck check details extraDep
      mSuggestion <$ traverse_ (report extraDep) mSuggestion

  pure $ length $ catMaybes $ concat results
 where
  extraDeps = filterExcludes oExcludes syExtraDeps
  resolver = fromMaybe syResolver oResolver
  checks = checksByName oChecks

filterExcludes :: [Pattern] -> [ExtraDep] -> [ExtraDep]
filterExcludes excludes = filter $ \e -> not $ any (`matchExclude` e) excludes
