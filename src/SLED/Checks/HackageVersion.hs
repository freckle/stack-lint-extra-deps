module SLED.Checks.HackageVersion
  ( checkHackageVersion
  ) where

import SLED.Prelude

import SLED.Check

checkHackageVersion :: Check
checkHackageVersion = Check $ \ed extraDep -> do
  Hackage hed <- pure extraDep
  hv <- ed.hackageVersions
  current <- hed.version
  released <- headMaybe hv.normal
  UpdateHackageVersion released <$ guard (released > current)
