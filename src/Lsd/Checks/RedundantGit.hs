module Lsd.Checks.RedundantGit
  ( checkRedundantGit
  ) where

import RIO

import Lsd.Check
import Lsd.Version
import RIO.List (headMaybe, intersect, sortOn)

checkRedundantGit :: Check
checkRedundantGit = Check $ \ExternalDetails {..} extraDep -> do
  Git _ <- pure extraDep
  GitDetails {..} <- edGitDetails

  let
    versions = map snd $ takeWhile ((> 0) . fst) $ sortOn
      (Down . fst)
      gdCommitCountToVersionTags

  suggestHackage edHackageVersions versions <|> suggestGit versions

-- Attempt to suggest a version tag that exists on Hackage
suggestHackage :: Maybe HackageVersions -> [Version] -> Maybe Suggestion
suggestHackage hackageVersions versions = do
  HackageVersions {..} <- hackageVersions
  version <- headMaybe $ hvNormal `intersect` versions
  pure $ Suggestion
    { sAction = Replace
    , sDetails = "Newer version (" <> display version <> ") exists on Hackage"
    }

-- Fall-back for when we can't find Hackage info, so just suggest if there are
-- newer version-like tags
suggestGit :: [Version] -> Maybe Suggestion
suggestGit versions = do
  version <- headMaybe versions
  pure $ Suggestion
    { sAction = Replace
    , sDetails = "Newer, version-like tag (" <> display version <> ") exists"
    }
