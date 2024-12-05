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
import Control.Error.Util (hush)
import Control.Lens (to, views, (<>=), (^.))
import Data.List (partition)
import qualified Data.List.NonEmpty as NE
import Data.Yaml.Marked.Decode (decodeThrow)
import Data.Yaml.Marked.Replace
import SLED.Check
import SLED.Checks
import SLED.Checks.StackageResolver
import SLED.Context
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
  context <- execContextT options.contents $ runSuggestions options

  let changed = context ^. contentsL /= options.contents

  when (options.autoFix && changed) $ do
    logInfo $ "Re-writing fixed stack.yaml" :# ["path" .= options.path]
    writeFileBS options.path $ context ^. contentsL

  let (fixed, unfixed) = partition markedItem $ context ^. seenL . to toList

  logDebug
    $ "Suggestions found"
    :# [ "fixed" .= length fixed
       , "unfixed" .= length unfixed
       ]

  unless (null unfixed || options.noExit) $ do
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
  -> StateT Context m ()
runSuggestions options = untilNoneSeen $ do
  lseen <- gets $ views seenL length
  logDebug $ "Linting pass" :# ["seen" .= lseen]

  bs <- gets $ view contentsL
  stackYaml <- liftIO $ markedItem <$> decodeThrow decodeStackYaml options.path bs

  let
    resolver :: Marked StackageResolver
    resolver = maybe stackYaml.resolver (<$ stackYaml.resolver) options.resolver

  sequenceFirstSeen
    $ bool id (checkResolver options resolver :) options.checkResolver
    $ map (checkExtraDep resolver options) stackYaml.extraDeps

checkResolver
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadStackage m
     , MonadReader env m
     , HasLogger env
     )
  => Options
  -> Marked StackageResolver
  -> StateT Context m ()
checkResolver options = whenUnseen $ \mresolver -> do
  mSuggestion <- lift $ checkStackageResolver mresolver

  for_ mSuggestion $ \ms -> do
    outputSuggestion options ms
    tryFixSuggestion options ms

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
  -> StateT Context m ()
checkExtraDep mresolver options = whenUnseen $ \med -> do
  when (included $ markedItem med) $ do
    mSuggestion <- lift $ runChecks mresolver options.checks med

    for_ mSuggestion $ \ms -> do
      outputSuggestion options ms
      tryFixSuggestion options ms
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
  -> StateT Context m ()
outputSuggestion options suggestion = do
  logDebug
    $ ""
    :# [ "suggestion" .= suggestionPretty (markedItem suggestion)
       , "mark" .= getTargetMark suggestion
       ]

  formatted <-
    formatSuggestion
      <$> getCurrentDirectory
      <*> gets (view contentsL)
      <*> getColorsLogger
      <*> pure options.format
      <*> pure suggestion

  pushLoggerLn formatted

-- | Tries to fix the 'Suggestion' and updates 'Context' accordingly
--
-- If fixed, 'contents' is updated and a @'Marked' 'True'@ is placed in 'seen';
-- otherwise, a @'Marked' 'False'@ is.
tryFixSuggestion
  :: (MonadState Context m, IsTarget t)
  => Options
  -> Marked (Suggestion t)
  -> m ()
tryFixSuggestion options ms = do
  fixed <- rewriteContents $ \bs -> fromMaybe bs $ do
    guard options.autoFix
    hush $ runReplaces [suggestionReplace bs ms] bs

  seenL <>= pure (fixed <$ ms)

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
