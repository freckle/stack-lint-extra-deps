module Checks.RedundantHackage
  ( checkRedundantHackage
  ) where

import RIO

import Check

checkRedundantHackage :: Check
checkRedundantHackage = Check $ \ExternalDetails {..} extraDep -> do
  Hackage HackageExtraDep {..} <- pure extraDep
  StackageVersions {..} <- edStackageVersions
  current <- hedVersion

  guard $ svOnPage >= current

  pure $ Suggestion
    { sTarget = extraDep
    , sAction = Remove
    , sDetails =
      "Same or newer version ("
      <> display svOnPage
      <> ") is already in your resolver"
    }
