module SLED.Version
  ( Version
  , parseVersion
  , showVersion
  ) where

import SLED.Prelude

import qualified Data.List.NonEmpty as NE
import qualified Data.Version as V
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)

newtype Version = Version V.Version
  deriving newtype
    ( Show
    , Eq
    , Ord
    , FromJSON
    )

instance ToJSON Version where
  toJSON (Version v) = toJSON $ "v" <> V.showVersion v
  toEncoding (Version v) = toEncoding $ "v" <> V.showVersion v

parseVersion :: String -> Maybe Version
parseVersion = fmap Version . parse V.parseVersion

showVersion :: Version -> String
showVersion (Version v) = V.showVersion v

parse :: ReadP a -> String -> Maybe a
parse p s = fst . NE.last <$> NE.nonEmpty (readP_to_S p s)
