module SLED.Suggestion.Format.GHA
  ( formatSuggestionGHA
  ) where

import SLED.Prelude

import Blammo.Logging.Colors
import qualified Data.Text as T
import SLED.Suggestion
import SLED.Suggestion.Format.Action

-- | Format a suggestion to create an annotation in GitHub Actions
--
-- @
-- ::error file={name},line={line},endLine={endLine},title={title}::{message}
-- @
formatSuggestionGHA :: Suggestion -> Text
formatSuggestionGHA s = "::error " <> T.intercalate "," attrs <> "::" <> s.reason
 where
  attrs =
    [ "file=" <> pack path
    , "line=" <> show startLine
    , "endLine=" <> show endLine
    , "title=" <> formatAction noColors s.action
    ]
  path = markedPath m
  startLine = locationLine $ markedLocationStart m
  endLine = locationLine $ markedLocationStart m
  m = suggestionLocation s
