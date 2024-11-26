module SLED.ExtraDep
  ( ExtraDep (..)
  , decodeExtraDep
  , matchPattern
  ) where

import SLED.Prelude

import Control.Error.Util (hush)
import Data.Yaml.Marked.Value
import SLED.GitExtraDep
import SLED.HackageExtraDep
import SLED.PackageName
import System.FilePath.Glob

data ExtraDep
  = -- | @{package}-{version(\@rev:{number})}@
    Hackage HackageExtraDep
  | -- | @{ git: {repository}, commit: {commit} }@
    Git GitExtraDep
  | -- | Local path, or any future style Stack adds
    Other ()
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

decodeExtraDep :: Marked Value -> Either String (Marked ExtraDep)
decodeExtraDep mv =
  Right
    $ fromMaybe (Other () <$ mv)
    $ asum
      [ hush (Git <$$> decodeGitExtraDep mv)
      , hush (Hackage <$$> decodeHackageExtraDep mv)
      ]

matchPattern :: Pattern -> ExtraDep -> Bool
matchPattern p = \case
  Hackage hed -> p `match` unpack hed.package.unwrap
  Git ged -> p `match` unpack (repositoryBase ged.repository)
  Other {} -> False
