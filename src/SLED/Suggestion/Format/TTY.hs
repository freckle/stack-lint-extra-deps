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

  gutterLine :: Maybe Int -> Text
  gutterLine mn = blue $ padTo sideBarWidth (maybe " " show mn) <> "|"

  contentLines =
    concat
      [ [""]
      , T.lines
          $ slice bs (startOfStartLine + 1) markedStart
          <> wrapNonSpace red (slice bs markedStart markedEnd)
          <> slice bs (markedEnd + 1) endOfEndLine
      , [markerLine]
      ]

  markerLine =
    T.replicate minColumn " "
      <> red (T.replicate markedWidth "^")

  sideBarWidth = length (show @String endLine) + 1

  startOfStartLine =
    fromMaybe 0
      $ find (\n -> BS8.indexMaybe bs n == Just '\n')
      $ reverse [0 .. markedStart]

  endOfEndLine =
    fromMaybe (BS8.length bs)
      $ find (\n -> BS8.indexMaybe bs n == Just '\n') [markedEnd ..]

  (minColumn, maxColumn) =
    (NE.minimum1 &&& NE.maximum1) $ startColumn :| [endColumn]

  markedWidth = maxColumn - minColumn

  startLine :: Int
  startLine = (+ 1) $ fromIntegral $ locationLine $ markedLocationStart m

  endLine :: Int
  endLine = (+ 1) $ fromIntegral $ locationLine $ markedLocationEnd m

  startColumn :: Int
  startColumn = fromIntegral $ locationColumn $ markedLocationStart m

  endColumn :: Int
  endColumn = fromIntegral $ locationColumn $ markedLocationEnd m

  markedStart :: Int
  markedStart = fromIntegral $ locationIndex $ markedLocationStart m

  markedEnd :: Int
  markedEnd = fromIntegral $ locationIndex $ markedLocationEnd m

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
padTo :: Int -> Text -> Text
padTo n t =
  let l = T.length t
  in  if T.length t < n
        then t <> T.replicate (n - l) " "
        else T.take n t

-- | Slices between start (inclusive) and end (exclusive) indexes
--
-- Returns 'Text' because that's what we need here.
slice :: ByteString -> Int -> Int -> Text
slice bs start end = pack $ map (BS8.index bs) [start .. (end - 1)]
