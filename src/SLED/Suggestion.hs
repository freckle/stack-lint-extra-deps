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

instance Semigroup SuggestionAction where
  -- No use doing other changes if we're going to remove a thing
  a@Remove <> _ = a
  _ <> b@Remove = b
  -- No use updating a commit if we're going to replace it
  a@ReplaceGitWithHackage {} <> UpdateGitCommit {} = a
  UpdateGitCommit {} <> b@ReplaceGitWithHackage {} = b
  -- Other combinations are not possible (e.g. you could never suggestion a
  -- hackage version update and a git commit update on the same dep), but we
  -- can't show that in types. We'll just make the catch-all use Last semantics.
  _ <> b = b

suggestionActionDescription :: SuggestionAction -> Text
suggestionActionDescription = \case
  Remove {} -> "This version (or newer) is in your Stackage resolver"
  UpdateGitCommit {} -> "Newer commits exist on the default branch"
  UpdateHackageVersion {} -> "A newer version is available"
  ReplaceGitWithHackage {} -> "A version on Hackage exists for this commit (or newer)"
