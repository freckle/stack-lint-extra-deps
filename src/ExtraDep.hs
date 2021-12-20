module ExtraDep
  ( ExtraDep(..)
  , matchPattern
  ) where

import RIO

import Data.Aeson
import GitExtraDep
import HackageExtraDep
import PackageName
import RIO.Text (unpack)
import System.FilePath.Glob

data ExtraDep
    = Hackage HackageExtraDep
    -- ^ @{package}(-{version})(@{checksum})
    | Git GitExtraDep
    -- ^ @{ git: {repository}, commit: {commit} }@
    | Other Value
    -- ^ Local path, or any future style Stack adds
    deriving stock Show

instance FromJSON ExtraDep where
  parseJSON x =
    asum [Git <$> parseJSON x, Hackage <$> parseJSON x, pure $ Other x]

instance Display ExtraDep where
  display = \case
    Hackage x -> display x
    Git x -> display x
    Other{} -> "<other>"

matchPattern :: Pattern -> ExtraDep -> Bool
matchPattern p = \case
  Hackage HackageExtraDep {..} -> p `match` unpack (unPackageName hedPackage)
  Git GitExtraDep {..} -> p `match` unpack (repositoryBase gedRepository)
  Other{} -> False
