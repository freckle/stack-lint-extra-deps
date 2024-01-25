{-# LANGUAGE RecordWildCards #-}

module SLED.Suggestion.Format.Action
  ( formatAction
  ) where

import SLED.Prelude

import Blammo.Logging.Colors
import qualified Data.Text as T
import SLED.ExtraDep
import SLED.GitExtraDep
import SLED.HackageExtraDep
import SLED.PackageName
import SLED.Suggestion
import SLED.Version

formatAction :: Colors -> ExtraDep -> SuggestionAction -> Text
formatAction Colors {..} target = \case
  Remove ->
    green "Remove"
      <> " "
      <> magenta (formatExtraDep target)
  UpdateGitCommit sha ->
    yellow "Update"
      <> " "
      <> magenta (formatExtraDep target)
      <> " to "
      <> cyan sha.unwrap
  UpdateHackageVersion version ->
    yellow "Update"
      <> " "
      <> magenta (formatExtraDep target)
      <> " to "
      <> cyan (pack $ showVersion version)
  ReplaceGitWithHackage hed ->
    yellow "Replace"
      <> " "
      <> magenta (formatExtraDep target)
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
formatGitExtraDep ged =
  repositoryBase ged.repository
    <> "@"
    <> T.take 7 (markedItem ged.commit).unwrap
