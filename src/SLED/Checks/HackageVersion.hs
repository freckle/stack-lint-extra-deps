module SLED.Checks.HackageVersion
  ( checkHackageVersion
  ) where

import SLED.Prelude

import SLED.Check

checkHackageVersion :: Check
checkHackageVersion = Check $ \ed extraDep -> do
  Hackage hed <- pure $ markedItem extraDep
  hv <- ed.hackageVersions

  current <- hed.version
  released <- headMaybe hv.normal

  guard $ released > current

  pure
    $ Suggestion
      { action =
          UpdateHackageVersion (hed <$ extraDep) $ hed {version = Just released}
      , reason = "Newer version is available"
      }
