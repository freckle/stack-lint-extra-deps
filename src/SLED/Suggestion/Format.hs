module SLED.Suggestion.Format
  ( Format (..)
  , formatSuggestion
  ) where

import SLED.Prelude

import Blammo.Logging.Colors
import Data.Aeson (encode)
import SLED.Suggestion

data Format
  = FormatJSON
  | FormatTTY Colors

formatSuggestion :: Format -> Suggestion -> Text
formatSuggestion = \case
  FormatJSON -> decodeUtf8 . encode
  FormatTTY {} -> error "TODO"
