module SLED.ExtraDep
  ( ExtraDep (..)
  , extraDepToText
  , matchPattern
  ) where

import SLED.Prelude

import SLED.GitExtraDep
import SLED.HackageExtraDep
import SLED.PackageName
import System.FilePath.Glob

data ExtraDep
  = -- | @{package}(-{version})(@{checksum})
    Hackage HackageExtraDep
  | -- | @{ git: {repository}, commit: {commit} }@
    Git GitExtraDep
  | -- | Local path, or any future style Stack adds
    Other Value
  deriving stock (Show)

instance FromJSON ExtraDep where
  parseJSON x =
    asum [Git <$> parseJSON x, Hackage <$> parseJSON x, pure $ Other x]

extraDepToText :: ExtraDep -> Text
extraDepToText = \case
  Hackage x -> hackageExtraDepToText x
  Git x -> gitExtraDepToText x
  Other {} -> "<other>"

matchPattern :: Pattern -> ExtraDep -> Bool
matchPattern p = \case
  Hackage HackageExtraDep {..} -> p `match` unpack (unPackageName hedPackage)
  Git GitExtraDep {..} -> p `match` unpack (repositoryBase gedRepository)
  Other {} -> False
