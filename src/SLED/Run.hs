module SLED.Run
  ( runLsd
  ) where

import SLED.Prelude

import SLED.Check
import SLED.Checks
import SLED.StackYaml
import SLED.StackageResolver
import System.FilePath.Glob

runLsd
  :: (MonadUnliftIO m, MonadLogger m, MonadReader env m)
  => StackYaml
  -> Maybe StackageResolver
  -> ChecksName
  -> Maybe Pattern
  -> [Pattern]
  -> (Suggestion -> m ())
  -> m Int
runLsd StackYaml {..} mResolver checksName mFilter excludes report = do
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
    filterExcludes excludes
      $ maybe id (filter . matchPattern) mFilter syExtraDeps
  resolver = fromMaybe syResolver mResolver
  checks = checksByName checksName

filterExcludes :: [Pattern] -> [ExtraDep] -> [ExtraDep]
filterExcludes excludes = filter $ \e -> not $ any (`matchPattern` e) excludes
