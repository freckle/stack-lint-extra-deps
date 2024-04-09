module SLED.ExtraDep
  ( ExtraDep (..)
  , decodeExtraDep
  , matchPattern
  ) where

import SLED.Prelude

import Data.Either.Extra (eitherToMaybe)
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
decodeExtraDep mv = Right $ fromMaybe (Other () <$ mv) $
  asum
    [ eitherToMaybe (Git <$$> decodeGitExtraDep mv)
    , eitherToMaybe (Hackage <$$> json mv)
    ]

matchPattern :: Pattern -> ExtraDep -> Bool
matchPattern p = \case
  Hackage hed -> p `match` unpack hed.package.unwrap
  Git ged -> p `match` unpack (repositoryBase ged.repository)
  Other {} -> False
