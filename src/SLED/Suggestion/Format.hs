module SLED.Suggestion.Format
  ( Format
  , readFormat
  , showFormat
  , defaultFormat
  , formatSuggestion
  ) where

import SLED.Prelude

import Blammo.Logging.Colors
import Data.Aeson (encode)
import SLED.Suggestion
import SLED.Suggestion.Format.GHA
import SLED.Suggestion.Format.TTY

data Format
  = FormatJSON
  | FormatTTY
  | FormatGHA

readFormat :: String -> Either String Format
readFormat = \case
  "json" -> Right FormatJSON
  "tty" -> Right FormatTTY
  "gha" -> Right FormatGHA
  x -> Left $ "Invalid format, " <> x <> " must be tty|gha|json"

showFormat :: Format -> String
showFormat = \case
  FormatJSON -> "json"
  FormatTTY -> "tty"
  FormatGHA -> "gha"

defaultFormat :: Format
defaultFormat = FormatTTY

formatSuggestion
  :: FilePath
  -- ^ Current directory
  -> ByteString
  -- ^ Full content of file being linted
  -> Colors
  -> Format
  -> Suggestion
  -> Text
formatSuggestion cwd bs colors = \case
  FormatJSON -> decodeUtf8 . encode
  FormatTTY -> formatSuggestionTTY cwd bs colors
  FormatGHA -> formatSuggestionGHA
