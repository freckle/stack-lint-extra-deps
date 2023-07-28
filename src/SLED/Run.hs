module SLED.Run
  ( runLsd
  ) where

import SLED.Prelude

import Blammo.Logging.Colors
import Blammo.Logging.Logger
import SLED.Check
import SLED.Checks
import SLED.StackYaml
import SLED.StackageResolver
import System.FilePath.Glob

runLsd
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadStackYaml m
     , MonadReader env m
     , HasLogger env
     )
  => FilePath
  -> Maybe StackageResolver
  -> ChecksName
  -> Maybe Pattern
  -> [Pattern]
  -> m Int
runLsd path mResolver checksName mFilter excludes = do
  StackYaml {..} <- loadStackYaml path
  Colors {..} <- getColorsLogger

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
      with (runCheck check details extraDep) $ \Suggestion {..} ->
        pushLoggerLn
          $ case sAction of
            Remove ->
              green "Remove " <> " " <> magenta (extraDepToText sTarget)
            ReplaceWith r ->
              yellow "Replace"
                <> " "
                <> magenta (extraDepToText sTarget)
                <> " with "
                <> cyan (extraDepToText r)
          <> "\n        ↳ "
          <> sDescription

  pure $ length $ catMaybes $ concat results

filterExcludes :: [Pattern] -> [ExtraDep] -> [ExtraDep]
filterExcludes excludes = filter $ \e -> not $ any (`matchPattern` e) excludes

with :: (Foldable t, Applicative f) => t a -> (a -> f b) -> f (t a)
with x f = x <$ for_ x f
