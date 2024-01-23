module SLED.Checks.RedundantHackage
  ( checkRedundantHackage
  ) where

import SLED.Prelude

import SLED.Check

checkRedundantHackage :: Check
checkRedundantHackage = Check $ \ed extraDep -> do
  Hackage hed <- pure $ markedItem extraDep
  sv <- ed.stackageVersions
  current <- hed.version

  guard $ sv.onPage >= current

  pure
    $ Suggestion
      { target = extraDep
      , action = Remove
      , description = "Same or newer version is now in your resolver"
      }
