{-# LANGUAGE RecordWildCards #-}

module SLED.Suggestion.Format.TTY
  ( formatSuggestionTTY
  ) where

import SLED.Prelude

import Blammo.Logging.Colors
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isSpace)
import qualified Data.List.NonEmpty.Extra as NE
import qualified Data.Text as T
import SLED.Marked.Line
import SLED.Suggestion
import SLED.Suggestion.Format.Action
import SLED.Suggestion.Format.Target
import System.FilePath (pathSeparator)

formatSuggestionTTY
  :: IsTarget t => FilePath -> ByteString -> Colors -> Marked (Suggestion t) -> Text
formatSuggestionTTY cwd bs colors@Colors {..} m =
  T.unlines
    $ [ formatAction colors s.target s.action
      , "  ├ "
          <> pack (cwd <> [pathSeparator])
          <> bold (formatMarkedLocation m)
      ]
    <> map ("  │ " <>) (formatMarkedContentIn colors contentMark bs)
    <> [ "  │ "
       , "  └ " <> suggestionActionDescription s.action
       ]
 where
  s = markedItem m
  contentMark = getTargetMark m

formatMarkedLocation :: Marked a -> Text
formatMarkedLocation m =
  pack (markedPath m)
    <> ":"
    <> pack (show $ (+ 1) $ locationLine $ markedLocationStart m)
    <> ":"
    <> pack (show $ (+ 1) $ locationColumn $ markedLocationStart m)

formatMarkedContentIn :: Colors -> Marked a -> ByteString -> [Text]
formatMarkedContentIn Colors {..} m bs =
  zipWith (<>) gutterLines contentLines
 where
  gutterLines =
    map gutterLine
      $ [Nothing]
      <> map Just [startLine .. endLine]
      <> [Nothing]

  gutterLine :: Maybe Natural -> Text
  gutterLine mn = blue $ padTo sideBarWidth (maybe " " show mn) <> "|"

  contentLines =
    concat
      [ [""]
      , T.lines
          $ slice bs (startOfStartLine bs m) (markedStart m)
          <> wrapNonSpace red (slice bs (markedStart m) (markedEnd m))
          <> slice bs (markedEnd m) (endOfEndLine bs m)
      , [markerLine]
      ]

  markerLine =
    T.replicate (fromIntegral minColumn) " "
      <> red (T.replicate (fromIntegral markedWidth) "^")

  sideBarWidth = genericLength @Natural (show @String endLine) + 1

  (minColumn, maxColumn) =
    (NE.minimum1 &&& NE.maximum1) $ startColumn :| [endColumn]

  markedWidth = maxColumn - minColumn

  startLine :: Natural
  startLine = (+ 1) $ locationLine $ markedLocationStart m

  endLine :: Natural
  endLine = (+ 1) $ locationLine $ markedLocationEnd m

  startColumn :: Natural
  startColumn = locationColumn $ markedLocationStart m

  endColumn :: Natural
  endColumn = locationColumn $ markedLocationEnd m

-- | Break and re-join a 'Text', converting non-space segments by a function
wrapNonSpace :: (Text -> Text) -> Text -> Text
wrapNonSpace f t
  | T.null b = a
  | otherwise = a <> f c <> wrapNonSpace f d
 where
  (a, b) = T.break (not . isSpace) t
  (c, d) = T.break isSpace b

-- | Pads a 'Text' with spaces up to the given length
--
-- If the given value is longer, it is truncated.
padTo :: Natural -> Text -> Text
padTo n t
  | T.length t < i = t <> T.replicate (i - l) " "
  | otherwise = T.take i t
 where
  i = fromIntegral @_ @Int n
  l = T.length t

-- | Slices between start (inclusive) and end (exclusive) indexes
--
-- Returns 'Text' because that's what we need here.
slice :: ByteString -> Natural -> Natural -> Text
slice bs start end =
  pack $ map (BS8.index bs) [fromIntegral start .. (fromIntegral end - 1)]
