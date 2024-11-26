-- | Helpers for dealing with 'Marked' lines
module SLED.Marked.Line
  ( startOfStartLine
  , endOfEndLine
  , markedStart
  , markedEnd
  ) where

import SLED.Prelude

import qualified Data.ByteString.Char8 as BS8
import Numeric.Natural (minusNaturalMaybe)

-- | Locate the start index of the start of a mark's line
--
-- This will be the index of the character /after/ the nearest newline
-- backwards. This will return zero if there is no newline, or if the bytestring
-- is empty.
startOfStartLine :: ByteString -> Marked a -> Natural
startOfStartLine bs m = maybe 0 (+ 1) $ findNewline bs $ reverse [0 .. markedStart m]

-- | Locate the end index of the end of a mark's line
--
-- This will be the index /of/ the nearest newline forwards. This will return
-- the last index in the bytestring if there is no newline, and will return 0 if
-- the bytestring is empty.
endOfEndLine :: ByteString -> Marked a -> Natural
endOfEndLine bs m = fromMaybe endIdx $ findNewline bs [markedEnd m .. endIdx]
 where
  endIdx :: Natural
  endIdx = fromMaybe 0 $ fromIntegral (BS8.length bs) `minusNaturalMaybe` 1

findNewline :: ByteString -> [Natural] -> Maybe Natural
findNewline bs = find (\n -> BS8.indexMaybe bs (fromIntegral n) == Just '\n')

markedStart :: Marked a -> Natural
markedStart = locationIndex . markedLocationStart

markedEnd :: Marked a -> Natural
markedEnd = locationIndex . markedLocationEnd
