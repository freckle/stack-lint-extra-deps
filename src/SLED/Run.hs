module SLED.Run
  ( runLsd

    -- * Exported for testing
  , shouldIncludeExtraDep
  , runChecks
  ) where

import SLED.Prelude

import Blammo.Logging.Colors
import Blammo.Logging.Logger
import Conduit
import Data.Conduit.Combinators (iterM)
import SLED.Check
import SLED.Checks
import SLED.StackYaml
import SLED.StackageResolver
import System.FilePath.Glob

runLsd
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadStackYaml m
     , MonadHackage m
     , MonadStackage m
     , MonadReader env m
     , HasLogger env
     )
  => FilePath
  -> Maybe StackageResolver
  -> ChecksName
  -> Maybe Pattern
  -> [Pattern]
  -> m Int
runLsd path mResolver checksName mInclude excludes = do
  StackYaml {..} <- loadStackYaml path
  let resolver = fromMaybe syResolver mResolver

  runConduit
    $ yieldMany syExtraDeps
    .| filterC (shouldIncludeExtraDep mInclude excludes)
    .| awaitForever
      ( \extraDep -> do
          suggestions <- lift $ runChecks resolver checksName extraDep
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
     , MonadLogger m
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

passC :: Monad m => ConduitT a a m ()
passC = awaitForever yield
