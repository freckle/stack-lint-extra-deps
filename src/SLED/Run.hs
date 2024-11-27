{-# LANGUAGE ViewPatterns #-}

module SLED.Run
  ( runSLED

    -- * Exported for testing
  , shouldIncludeExtraDep
  , runSuggestions
  ) where

import SLED.Prelude

import Blammo.Logging.Colors
import Blammo.Logging.Logger
import qualified Data.List.NonEmpty as NE
import Data.Yaml.Marked.Decode (decodeThrow)
import Data.Yaml.Marked.Replace
import SLED.Check
import SLED.Checks
import SLED.Checks.StackageResolver
import SLED.Marked.Line
import SLED.Options.Parse
import SLED.StackYaml
import SLED.StackageResolver
import SLED.Suggestion.Format
import SLED.Suggestion.Format.Target
import System.FilePath.Glob
import UnliftIO.Directory (getCurrentDirectory)

runSLED
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadHackage m
     , MonadStackage m
     , MonadGit m
     , MonadReader env m
     , HasLogger env
     , HasCallStack
     )
  => Options
  -> m ()
runSLED options = do
  replaces <- runSuggestions options

  let changed = not $ null replaces

  when (options.autoFix && changed) $ do
    logInfo $ "Re-writing fixed stack.yaml" :# ["path" .= options.path]
    liftIO $ writeFileBS options.path =<< runReplaces replaces options.contents

  logDebug $ "Suggestions found" :# ["count" .= length replaces]

  unless (not changed || options.noExit) $ do
    logDebug "Exiting non-zero (--no-exit to disable)"
    exitFailure

runSuggestions
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadHackage m
     , MonadStackage m
     , MonadGit m
     , MonadReader env m
     , HasLogger env
     )
  => Options
  -> m [Replace]
runSuggestions options = do
  stackYaml <-
    liftIO
      $ maybe id setStackYamlResolver options.resolver
      . markedItem
      <$> decodeThrow decodeStackYaml options.path options.contents

  result <- checkResolver stackYaml options

  case result of
    Unchanged -> checkExtraDeps stackYaml options
    Updated yaml replace -> (replace :) <$> checkExtraDeps yaml options

data CheckResolverResult = Unchanged | Updated StackYaml Replace

checkResolver
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadStackage m
     , MonadReader env m
     , HasLogger env
     )
  => StackYaml
  -> Options
  -> m CheckResolverResult
checkResolver stackYaml options
  | not options.checkResolver = pure Unchanged
  | otherwise = do
      mSuggestion <- checkStackageResolver $ stackYaml.resolver

      case mSuggestion of
        Just ms | Suggestion _ (ReplaceWith latest) <- markedItem ms -> do
          outputSuggestion options ms

          let
            updated = setStackYamlResolver latest stackYaml
            replace = suggestionReplace options.contents ms

          pure $ Updated updated replace
        _ -> pure Unchanged

checkExtraDeps
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadHackage m
     , MonadStackage m
     , MonadGit m
     , MonadReader env m
     , HasLogger env
     )
  => StackYaml
  -> Options
  -> m [Replace]
checkExtraDeps stackYaml options =
  mapMaybeM (checkExtraDep stackYaml.resolver options) stackYaml.extraDeps

checkExtraDep
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadHackage m
     , MonadStackage m
     , MonadGit m
     , MonadReader env m
     , HasLogger env
     )
  => Marked StackageResolver
  -> Options
  -> Marked ExtraDep
  -> m (Maybe Replace)
checkExtraDep mresolver options med = do
  if included $ markedItem med
    then do
      mSuggestion <- runChecks mresolver options.checks med

      for mSuggestion $ \ms -> do
        outputSuggestion options ms
        pure $ suggestionReplace options.contents ms
    else
      pure Nothing
 where
  included = shouldIncludeExtraDep options.filter options.excludes

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
  -> m (Maybe (Marked (Suggestion ExtraDep)))
runChecks (markedItem -> resolver) checksName m@(markedItem -> extraDep) = do
  logDebug $ "Fetching external details" :# ["dependency" .= extraDep]

  details <-
    getExternalDetails resolver extraDep

  let mactions =
        NE.nonEmpty
          $ mapMaybe (\check -> check.run details extraDep)
          $ checksByName checksName

  for mactions $ \actions -> do
    -- Making multiple suggestions on one extra-dep is conflicting.
    -- We'll use the semigroup instance to give precendence and fold
    -- them down to one per extra-dep.
    let suggestion = Suggestion {target = extraDep, action = sconcat actions}

    -- And mark the Suggestion at the point of the ExtraDep
    pure $ suggestion <$ m

-- | Print 'Suggestion' according to 'Format'
outputSuggestion
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     , IsTarget t
     , ToJSON t
     )
  => Options
  -> Marked (Suggestion t)
  -> m ()
outputSuggestion options suggestion = do
  logDebug
    $ ""
    :# [ "suggestion" .= suggestionPretty (markedItem suggestion)
       , "mark" .= getTargetMark suggestion
       ]

  formatted <-
    formatSuggestion
      <$> getCurrentDirectory
      <*> pure options.contents
      <*> getColorsLogger
      <*> pure options.format
      <*> pure suggestion

  pushLoggerLn formatted

suggestionReplace
  :: IsTarget t => ByteString -> Marked (Suggestion t) -> Replace
suggestionReplace bs m = case s.action of
  Remove -> removeMarkedLine bs m
  UpdateGitCommit commit -> replaceMarkedTarget m commit
  UpdateHackageVersion version -> replaceMarkedTarget m version
  ReplaceGitWithHackage hed -> replaceMarkedTarget m hed
  ReplaceWith b -> replaceMarkedTarget m b
 where
  s = markedItem m

removeMarkedLine :: ByteString -> Marked a -> Replace
removeMarkedLine bs m = newReplace s len ""
 where
  s = startOfStartLine bs m
  e = endOfEndLine bs m
  len = e - s + 1 -- chomp newline too

suggestionPretty :: IsTarget t => Suggestion t -> Text
suggestionPretty s =
  formatTarget s.target <> ": " <> suggestionActionPretty s.action

suggestionActionPretty :: IsTarget t => SuggestionAction t -> Text
suggestionActionPretty = \case
  Remove -> "remove"
  UpdateGitCommit sha -> "updated to " <> formatTarget sha
  UpdateHackageVersion v -> "update to " <> formatTarget v
  ReplaceGitWithHackage d -> "update to " <> formatTarget d
  ReplaceWith t -> "replace with " <> formatTarget t
