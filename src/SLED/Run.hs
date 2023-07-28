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
  :: (MonadUnliftIO m, MonadLogger m, MonadStackYaml m)
  => FilePath
  -> Maybe StackageResolver
  -> ChecksName
  -> Maybe Pattern
  -> [Pattern]
  -> (Suggestion -> m ())
  -> m Int
runLsd path mResolver checksName mFilter excludes report = do
  StackYaml {..} <- loadStackYaml path

  let
    resolver = fromMaybe syResolver mResolver
    extraDeps =
      filterExcludes excludes
        $ maybe id (filter . matchPattern) mFilter syExtraDeps

  results <- for extraDeps $ \extraDep -> do
    logDebug
      $ "Fetching external details"
      :# ["dependency" .= extraDepToText extraDep]
    details <- getExternalDetails resolver extraDep

    for (checksByName checksName) $ \check -> do
      let mSuggestion = runCheck check details extraDep
      mSuggestion <$ traverse_ report mSuggestion

  pure $ length $ catMaybes $ concat results

filterExcludes :: [Pattern] -> [ExtraDep] -> [ExtraDep]
filterExcludes excludes = filter $ \e -> not $ any (`matchPattern` e) excludes
