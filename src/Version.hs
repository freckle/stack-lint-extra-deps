module Version
  ( Version
  , parseVersion
  , showVersion
  ) where

import RIO

import Data.Aeson
import qualified Data.Version as V
import qualified RIO.NonEmpty as NE
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)

newtype Version = Version V.Version
  deriving newtype
    ( Show
    , Eq
    , Ord
    , FromJSON
    )

instance Display Version where
  display (Version v) = fromString $ V.showVersion v

parseVersion :: String -> Maybe Version
parseVersion = fmap Version . parse V.parseVersion

showVersion :: Version -> String
showVersion (Version v) = V.showVersion v

parse :: ReadP a -> String -> Maybe a
parse p s = fst . NE.last <$> NE.nonEmpty (readP_to_S p s)
