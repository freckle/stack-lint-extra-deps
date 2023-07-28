module SLED.ExtraDep
  ( ExtraDep (..)
  , matchPattern
  ) where

import RIO

import Data.Aeson
import RIO.Text (unpack)
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

instance Display ExtraDep where
  display = \case
    Hackage x -> display x
    Git x -> display x
    Other {} -> "<other>"

matchPattern :: Pattern -> ExtraDep -> Bool
matchPattern p = \case
  Hackage HackageExtraDep {..} -> p `match` unpack (unPackageName hedPackage)
  Git GitExtraDep {..} -> p `match` unpack (repositoryBase gedRepository)
  Other {} -> False
