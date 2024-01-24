module SLED.Suggestion
  ( Suggestion (..)
  , SuggestionAction (..)
  , suggestionLocation
  ) where

import SLED.Prelude

import SLED.ExtraDep
import SLED.GitExtraDep
import SLED.HackageExtraDep

data SuggestionAction
  = Remove (Marked ExtraDep)
  | ReplaceCommit (Marked CommitSHA) CommitSHA
  | ReplaceGitWithHackage (Marked GitExtraDep) HackageExtraDep
  | UpdateHackageVersion (Marked HackageExtraDep) HackageExtraDep
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data Suggestion = Suggestion
  { action :: SuggestionAction
  , reason :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

suggestionLocation :: Suggestion -> Marked ()
suggestionLocation s = case s.action of
  Remove m -> void m
  ReplaceCommit m _ -> void m
  ReplaceGitWithHackage m _ -> void m
  UpdateHackageVersion m _ -> void m
