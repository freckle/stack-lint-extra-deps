module SLED.Checks.RedundantHackage
  ( checkRedundantHackage
  ) where

import SLED.Prelude

import SLED.Check
import SLED.Version

checkRedundantHackage :: Check
checkRedundantHackage = Check $ \ed extraDep -> do
  Hackage hed <- pure extraDep
  sv <- ed.stackageVersions
  let
    hvs = maybe [] (.normal) ed.hackageVersions
    current = defaultRevision hvs $ markedItem hed.version
  Remove <$ guard (sv.onPage >= current)
