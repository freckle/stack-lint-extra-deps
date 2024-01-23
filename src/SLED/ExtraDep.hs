module SLED.ExtraDep
  ( ExtraDep (..)
  , decodeExtraDep
  , matchPattern
  ) where

import SLED.Prelude

import Data.Yaml.Marked.Parse
import Data.Yaml.Marked.Value
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
    Other ()
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

decodeExtraDep :: Marked Value -> Either String (Marked ExtraDep)
decodeExtraDep mv =
  asum
    [ Git <$$> decodeGitExtraDep mv
    , Hackage <$$> json mv
    , pure $ Other () <$ mv
    ]

matchPattern :: Pattern -> ExtraDep -> Bool
matchPattern p = \case
  Hackage HackageExtraDep {..} -> p `match` unpack (unPackageName hedPackage)
  Git GitExtraDep {..} -> p `match` unpack (repositoryBase gedRepository)
  Other {} -> False
