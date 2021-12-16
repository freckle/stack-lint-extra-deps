module Lsd.Checks.RedundantGit
  ( checkRedundantGit
  ) where

import RIO

import Lsd.Check
import RIO.List (headMaybe, intersect, sortOn)

checkRedundantGit :: Check
checkRedundantGit = Check $ \ExternalDetails {..} extraDep -> do
  Git _ <- pure extraDep
  GitDetails {..} <- edGitDetails

  let
    versions = sortOn (Down . fst) gdCommitCountToVersionTags
    equalVersions = map snd $ takeWhile ((>= 0) . fst) versions
    newerVersions = map snd $ takeWhile ((> 0) . fst) versions

    -- Attempt to suggest a version tag that exists on Hackage
    suggestHackage = do
      HackageVersions {..} <- edHackageVersions
      version <- headMaybe $ hvNormal `intersect` equalVersions
      pure $ Suggestion
        { sAction = Replace
        , sDetails =
          "Same-or-newer version (" <> display version <> ") exists on Hackage"
        }

    -- Fall-back for when we can't find Hackage info, so just suggest if there
    -- are newer version-like tags
    suggestGit = do
      version <- headMaybe newerVersions
      pure $ Suggestion
        { sAction = Replace
        , sDetails =
          "Newer, version-like tag (" <> display version <> ") exists"
        }

  suggestHackage <|> suggestGit
