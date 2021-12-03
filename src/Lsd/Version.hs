module Lsd.Version
    ( Version
    , parseVersion
    , showVersion
    ) where

import RIO

import Data.Aeson
import qualified Data.Version as Version
import qualified RIO.NonEmpty as NE
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)

newtype Version = Version Version.Version
    deriving newtype
        ( Show
        , Eq
        , Ord
        , FromJSON
        )

instance Display Version where
    display (Version v) = fromString $ Version.showVersion v

parseVersion :: String -> Maybe Version
parseVersion = fmap Version . parse Version.parseVersion

showVersion :: Version -> String
showVersion (Version v) = Version.showVersion v

parse :: ReadP a -> String -> Maybe a
parse p s = fst . NE.last <$> NE.nonEmpty (readP_to_S p s)
