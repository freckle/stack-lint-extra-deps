module Suggestion
  ( Suggestion(..)
  , SuggestionAction(..)
  ) where

import RIO

import ExtraDep

data SuggestionAction
    = Remove
    | ReplaceWith ExtraDep

data Suggestion = Suggestion
  { sTarget :: ExtraDep
  , sAction :: SuggestionAction
  , sDetails :: Utf8Builder
  }
