module SLED.Run
  ( runSLED

    -- * Exported for testing
  , shouldIncludeExtraDep
  , runChecks
  ) where

import SLED.Prelude

import Blammo.Logging.Colors
import Blammo.Logging.Logger
import Conduit
import Data.Conduit.Combinators (iterM)
import qualified Data.Yaml as Yaml
import SLED.Check
import SLED.Checks
import SLED.Options
import SLED.StackYaml
import SLED.StackageResolver
import System.FilePath.Glob

runSLED
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadHackage m
     , MonadStackage m
     , MonadGit m
     , MonadReader env m
     , HasLogger env
     )
  => Options
  -> m Int
runSLED Options {..} = do
  logDebug $ "Loading stack.yaml" :# ["path" .= oPath]
  StackYaml {..} <- Yaml.decodeFileThrow oPath
  let resolver = fromMaybe syResolver oResolver

  runConduit
    $ yieldMany syExtraDeps
    .| filterC (shouldIncludeExtraDep oFilter oExcludes)
    .| awaitForever
      ( \extraDep -> do
          suggestions <- lift $ runChecks resolver oChecks extraDep
          yieldMany suggestions
      )
    .| printSuggestions
    .| lengthC

shouldIncludeExtraDep :: Maybe Pattern -> [Pattern] -> ExtraDep -> Bool
shouldIncludeExtraDep mInclude excludes dep
  | Just include <- mInclude, not (include `matchPattern` dep) = False
  | any (`matchPattern` dep) excludes = False
  | otherwise = True

runChecks
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadHackage m
     , MonadStackage m
     , MonadGit m
     )
  => StackageResolver
  -> ChecksName
  -> ExtraDep
  -> m [Suggestion]
runChecks resolver checksName extraDep = do
  logDebug
    $ "Fetching external details"
    :# ["dependency" .= extraDepToText extraDep]
  details <- getExternalDetails resolver extraDep

  pure
    $ mapMaybe (\check -> runCheck check details extraDep)
    $ checksByName checksName

printSuggestions
  :: ( MonadIO m
     , MonadReader env m
     , HasLogger env
     )
  => ConduitT Suggestion Suggestion m ()
printSuggestions = do
  Colors {..} <- lift getColorsLogger

  iterM $ \Suggestion {..} ->
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
