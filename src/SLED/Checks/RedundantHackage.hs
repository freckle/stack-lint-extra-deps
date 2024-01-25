module SLED.Checks.RedundantHackage
  ( checkRedundantHackage
  ) where

import SLED.Prelude

import SLED.Check

checkRedundantHackage :: Check
checkRedundantHackage = Check $ \ed extraDep -> do
  Hackage hed <- pure extraDep
  sv <- ed.stackageVersions
  current <- hed.version
  Remove <$ guard (sv.onPage >= current)
