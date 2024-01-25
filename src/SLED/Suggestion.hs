module SLED.Suggestion
  ( Suggestion (..)
  , SuggestionAction (..)
  , suggestionActionDescription
  ) where

import SLED.Prelude

import SLED.ExtraDep
import SLED.GitExtraDep
import SLED.HackageExtraDep
import SLED.Version

data Suggestion = Suggestion
  { target :: ExtraDep
  , action :: SuggestionAction
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data SuggestionAction
  = Remove
  | UpdateGitCommit CommitSHA
  | UpdateHackageVersion Version
  | ReplaceGitWithHackage HackageExtraDep
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

suggestionActionDescription :: SuggestionAction -> Text
suggestionActionDescription = \case
  Remove {} -> "This version (or newer) is in your Stackage resolver"
  UpdateGitCommit {} -> "Newer commits exist on the default branch"
  UpdateHackageVersion {} -> "A newer version is available"
  ReplaceGitWithHackage {} -> "A version on Hackage exists for this commit (or newer)"
