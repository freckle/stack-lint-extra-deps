{-# LANGUAGE RecordWildCards #-}

module SLED.Suggestion.Format.Action
  ( formatAction
  ) where

import SLED.Prelude

import Blammo.Logging.Colors
import SLED.GitExtraDep
import SLED.Suggestion
import SLED.Suggestion.Format.Target
import SLED.Version

formatAction :: IsTarget t => Colors -> t -> SuggestionAction t -> Text
formatAction Colors {..} target = \case
  Remove ->
    green "Remove"
      <> " "
      <> magenta (formatTarget target)
  UpdateGitCommit sha ->
    yellow "Update"
      <> " "
      <> magenta (formatTarget target)
      <> " to "
      <> cyan sha.unwrap
  UpdateHackageVersion version ->
    yellow "Update"
      <> " "
      <> magenta (formatTarget target)
      <> " to "
      <> cyan (pack $ showVersion version)
  ReplaceGitWithHackage hed ->
    yellow "Replace"
      <> " "
      <> magenta (formatTarget target)
      <> " with "
      <> cyan (formatTarget hed)
  ReplaceWith other ->
    yellow "Replace"
      <> " "
      <> magenta (formatTarget target)
      <> " with "
      <> cyan (formatTarget other)
