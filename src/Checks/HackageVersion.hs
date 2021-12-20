module Checks.HackageVersion
  ( checkHackageVersion
  ) where

import RIO

import Check
import RIO.List (headMaybe)

checkHackageVersion :: Check
checkHackageVersion = Check $ \ExternalDetails {..} extraDep -> do
  Hackage HackageExtraDep {..} <- pure extraDep
  HackageVersions {..} <- edHackageVersions

  current <- hedVersion
  released <- headMaybe hvNormal

  guard $ released > current

  pure $ Suggestion
    { sAction = Replace
    , sDetails = "Newer version (" <> display released <> ") is available"
    }
