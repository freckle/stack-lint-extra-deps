module SLED.Checks.RedundantGit
  ( checkRedundantGit
  ) where

import SLED.Prelude

import Data.List (intersect)
import SLED.Check
import SLED.PackageName

checkRedundantGit :: Check
checkRedundantGit = Check $ \ed extraDep -> do
  Git ged <- pure extraDep
  gd <- ed.gitDetails

  let
    versions = sortOn (Down . fst) gd.commitCountToVersionTags
    equalVersions = map snd $ takeWhile ((>= 0) . fst) versions
    newerVersions = map snd $ takeWhile ((> 0) . fst) versions
    replaceGitWithHackage v =
      ReplaceGitWithHackage
        HackageExtraDep
          { package = PackageName $ repositoryBaseName ged.repository
          , version = markAtZero v -- this mark is ignored
          }

    -- Attempt to suggest a version tag that exists on Hackage
    suggestHackage = do
      hv <- ed.hackageVersions
      version <- headMaybe $ hv.normal `intersect` equalVersions
      pure $ replaceGitWithHackage version

    -- Fall-back for when we can't find Hackage info, so just suggest if there
    -- are newer version-like tags
    suggestGit = do
      version <- headMaybe newerVersions
      pure $ replaceGitWithHackage version

  suggestHackage <|> suggestGit

markAtZero :: a -> Marked a
markAtZero a =
  Marked
    { markedItem = a
    , markedPath = "<input>"
    , markedLocationStart = Location 0 0 0
    , markedLocationEnd = Location 0 0 0
    }
