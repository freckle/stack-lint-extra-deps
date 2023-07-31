module SLED.Suggestion
  ( Suggestion (..)
  , SuggestionAction (..)
  ) where

import SLED.Prelude

import SLED.ExtraDep

data SuggestionAction
  = Remove
  | ReplaceWith ExtraDep
  deriving stock (Eq, Show)

data Suggestion = Suggestion
  { sTarget :: ExtraDep
  , sAction :: SuggestionAction
  , sDescription :: Text
  }
  deriving stock (Eq, Show)
