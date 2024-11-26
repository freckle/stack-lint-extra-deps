{-# LANGUAGE NamedFieldPuns #-}

module SLED.HackageExtraDep
  ( HackageExtraDep (..)
  , decodeHackageExtraDep
  ) where

import SLED.Prelude

import Data.Ord (clamp)
import qualified Data.Text as T
import Data.Yaml.Marked.Parse
import Data.Yaml.Marked.Value
import SLED.PackageName
import SLED.Parse
import SLED.Version

data HackageExtraDep = HackageExtraDep
  { package :: PackageName
  , version :: Marked Version
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

decodeHackageExtraDep :: Marked Value -> Either String (Marked HackageExtraDep)
decodeHackageExtraDep mv =
  withText "HackageExtraDep" (parseErr mkMessage go) mv
 where
  mkMessage :: Text -> String
  mkMessage raw = "extra-dep " <> show raw <> " did not parse as {package}-{version}"

  go = do
    package <- packageParser <* char '-'
    version <- versionParser

    let
      -- Set the start past the package + hypen
      shift :: Natural
      shift = fromIntegral (T.length package.unwrap + 1)

    pure
      $ HackageExtraDep
        { package
        , version = version <$ rshiftStart shift mv
        }

packageParser :: ReadP PackageName
packageParser = PackageName . pack <$> many1 anyChar

rshiftStart :: Natural -> Marked a -> Marked a
rshiftStart n m = m {markedLocationStart = newStart}
 where
  newStart = s {locationIndex = newIndex, locationColumn = newColumn}
  newIndex = clamp (0, locationIndex e) $ (+ n) $ locationIndex s
  newColumn = clamp (0, locationColumn e) $ (+ n) $ locationColumn s
  s = markedLocationStart m
  e = markedLocationEnd m
