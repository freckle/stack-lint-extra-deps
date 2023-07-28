module SLED.Run
  ( runLsd
  ) where

import SLED.Prelude

import SLED.Check
import SLED.Checks
import SLED.Options
import SLED.StackYaml
import System.FilePath.Glob

runLsd
  :: (MonadUnliftIO m, MonadLogger m, MonadReader env m)
  => Options
  -> StackYaml
  -> (Suggestion -> m ())
  -> m Int
runLsd Options {..} StackYaml {..} report = do
  results <- for extraDeps $ \extraDep -> do
    logDebug
      $ "Fetching external details"
      :# ["dependency" .= extraDepToText extraDep]
    details <- getExternalDetails resolver extraDep

    for checks $ \check -> do
      let mSuggestion = runCheck check details extraDep
      mSuggestion <$ traverse_ report mSuggestion

  pure $ length $ catMaybes $ concat results
 where
  extraDeps =
    filterExcludes oExcludes
      $ maybe id (filter . matchPattern) oFilter syExtraDeps
  resolver = fromMaybe syResolver oResolver
  checks = checksByName oChecks

filterExcludes :: [Pattern] -> [ExtraDep] -> [ExtraDep]
filterExcludes excludes = filter $ \e -> not $ any (`matchPattern` e) excludes
