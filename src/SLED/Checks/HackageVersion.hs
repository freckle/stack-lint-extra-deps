module SLED.Checks.HackageVersion
  ( checkHackageVersion
  ) where

import SLED.Prelude

import SLED.Check

checkHackageVersion :: Check
checkHackageVersion = Check $ \ExternalDetails {..} extraDep -> do
  Hackage hed@HackageExtraDep {..} <- pure $ markedItem extraDep
  HackageVersions {..} <- edHackageVersions

  current <- hedVersion
  released <- headMaybe hvNormal

  guard $ released > current

  pure
    $ Suggestion
      { sTarget = extraDep
      , sAction = replaceHackageDep (hed <$ extraDep) released
      , sDescription = "Newer version is available"
      }
