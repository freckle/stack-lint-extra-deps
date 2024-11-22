module SLED.Parse
  ( ReadP
  , parse
  , parseOr
  , string
  , char
  , anyChar
  , nat
  , hexes
  , many1
  )
where

import SLED.Prelude

import Data.Char (isDigit, isHexDigit)
import qualified Data.List.NonEmpty as NE
import Text.ParserCombinators.ReadP
  ( ReadP
  , char
  , eof
  , many1
  , readP_to_S
  , satisfy
  , string
  )
import qualified Prelude as Unsafe (read)

parse :: ReadP a -> Text -> Maybe a
parse p = fmap (fst . NE.last) . nonEmpty . readP_to_S (p <* eof) . unpack

parseOr :: ReadP a -> a -> Text -> a
parseOr p def = maybe def (fst . NE.last) . nonEmpty . readP_to_S (p <* eof) . unpack

nat :: ReadP Natural
nat = fmap Unsafe.read $ many1 $ satisfy isDigit

hexes :: ReadP String
hexes = many1 $ satisfy isHexDigit

anyChar :: ReadP Char
anyChar = satisfy $ const True
