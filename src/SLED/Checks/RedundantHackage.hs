module SLED.Checks.RedundantHackage
  ( checkRedundantHackage
  ) where

import SLED.Prelude

import SLED.Check

checkRedundantHackage :: Check
checkRedundantHackage = Check $ \ExternalDetails {..} extraDep -> do
  Hackage HackageExtraDep {..} <- pure $ markedItem extraDep
  StackageVersions {..} <- edStackageVersions
  current <- hedVersion

  guard $ svOnPage >= current

  pure
    $ Suggestion
      { sTarget = extraDep
      , sAction = Remove
      , sDescription = "Same or newer version is now in your resolver"
      }
