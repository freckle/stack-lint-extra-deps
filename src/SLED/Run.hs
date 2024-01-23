module SLED.Run
  ( runSLED

    -- * Exported for testing
  , shouldIncludeExtraDep
  , runChecks
  ) where

import SLED.Prelude

import Blammo.Logging.Logger
import Conduit
import Data.Conduit.Combinators (iterM)
import Data.Yaml.Marked.Decode
import SLED.Check
import SLED.Checks
import SLED.Options
import SLED.StackYaml
import SLED.StackageResolver
import SLED.Suggestion.Format
import System.FilePath.Glob

runSLED
  :: ( MonadThrow m
     , MonadUnliftIO m
     , MonadLogger m
     , MonadHackage m
     , MonadStackage m
     , MonadGit m
     , MonadReader env m
     , HasLogger env
     )
  => Options
  -> m ()
runSLED Options {..} = do
  logDebug $ "Loading stack.yaml" :# ["path" .= oPath]
  bs <- readFileBS oPath
  StackYaml {..} <-
    liftIO $ markedItem <$> decodeThrow decodeStackYaml oPath bs

  let
    -- Mark an option resolver with the in-file resolver's position so that if
    -- we do anything based on it, that's what we'll use
    resolver = maybe syResolver (<$ syResolver) oResolver

    -- TODO: Options
    format = FormatJSON

  suggestions <-
    runConduit
      $ yieldMany syExtraDeps
      .| filterC (shouldIncludeExtraDep oFilter oExcludes . markedItem)
      .| awaitForever
        ( \extraDep -> do
            suggestions <- lift $ runChecks resolver oChecks extraDep
            yieldMany suggestions
        )
      .| iterM (pushLoggerLn . formatSuggestion format)
      .| sinkList

  let n = length suggestions
  logDebug $ "Suggestions found" :# ["count" .= n]

  when (n /= 0 && not oNoExit) $ do
    logDebug "Exiting non-zero (--no-exit to disable)"
    exitFailure

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
  => Marked StackageResolver
  -> ChecksName
  -> Marked ExtraDep
  -> m [Suggestion]
runChecks resolver checksName extraDep = do
  logDebug
    $ "Fetching external details"
    :# ["dependency" .= markedItem extraDep]
  details <-
    getExternalDetails (markedItem resolver) $ markedItem extraDep

  pure
    $ mapMaybe (\check -> runCheck check details extraDep)
    $ checksByName checksName
