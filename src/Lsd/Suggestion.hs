module Lsd.Suggestion
    ( Suggestion(..)
    , SuggestionAction(..)
    ) where

import RIO

data SuggestionAction
    = Remove
    | Replace

data Suggestion = Suggestion
    { sAction :: SuggestionAction
    , sDetails :: Utf8Builder
    }
