module Checks.HackageVersion
  ( checkHackageVersion
  ) where

import RIO

import Check
import RIO.List (headMaybe)

checkHackageVersion :: Check
checkHackageVersion = Check $ \ExternalDetails {..} extraDep -> do
  Hackage hed@HackageExtraDep {..} <- pure extraDep
  HackageVersions {..} <- edHackageVersions

  current <- hedVersion
  released <- headMaybe hvNormal

  guard $ released > current

  pure $ Suggestion
    { sTarget = extraDep
    , sAction = ReplaceWith $ Hackage $ hed { hedVersion = Just released }
    , sDescription = "Newer version is available"
    }
