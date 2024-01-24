{-# LANGUAGE RecordWildCards #-}

module SLED.Suggestion.Format.Action
  ( formatAction
  ) where

import SLED.Prelude

import Blammo.Logging.Colors
import SLED.ExtraDep
import SLED.GitExtraDep
import SLED.HackageExtraDep
import SLED.PackageName
import SLED.Suggestion
import SLED.Version

formatAction :: Colors -> SuggestionAction -> Text
formatAction Colors {..} = \case
  Remove med ->
    green "Remove"
      <> "  "
      <> magenta (formatExtraDep $ markedItem med)
  ReplaceCommit msha sha ->
    yellow "Replace"
      <> " "
      <> magenta (markedItem msha).unwrap
      <> " with "
      <> cyan sha.unwrap
  ReplaceGitWithHackage mged hed ->
    yellow "Replace"
      <> " "
      <> magenta (formatGitExtraDep $ markedItem mged)
      <> " with "
      <> cyan (formatHackageExtraDep hed)
  UpdateHackageVersion mhed hed ->
    yellow "Replace"
      <> " "
      <> magenta (formatHackageExtraDep $ markedItem mhed)
      <> " with "
      <> cyan (formatHackageExtraDep hed)

formatExtraDep :: ExtraDep -> Text
formatExtraDep = \case
  Hackage hed -> formatHackageExtraDep hed
  Git ged -> formatGitExtraDep ged
  Other {} -> "<unknown>"

formatHackageExtraDep :: HackageExtraDep -> Text
formatHackageExtraDep hed =
  hed.package.unwrap <> maybe "" (("-" <>) . pack . showVersion) hed.version

formatGitExtraDep :: GitExtraDep -> Text
formatGitExtraDep _ = "example/git@abc123"
