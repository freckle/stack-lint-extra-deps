module Checks.RedundantGit
  ( checkRedundantGit
  ) where

import RIO

import Check
import PackageName
import RIO.List (headMaybe, intersect, sortOn)

checkRedundantGit :: Check
checkRedundantGit = Check $ \ExternalDetails {..} extraDep -> do
  Git GitExtraDep {..} <- pure extraDep
  GitDetails {..} <- edGitDetails

  let
    versions = sortOn (Down . fst) gdCommitCountToVersionTags
    equalVersions = map snd $ takeWhile ((>= 0) . fst) versions
    newerVersions = map snd $ takeWhile ((> 0) . fst) versions

    replaceWith v = ReplaceWith $ Hackage HackageExtraDep
      { hedPackage = packageName $ repositoryBaseName gedRepository
      , hedVersion = Just v
      , hedChecksum = Nothing
      }

    -- Attempt to suggest a version tag that exists on Hackage
    suggestHackage = do
      HackageVersions {..} <- edHackageVersions
      version <- headMaybe $ hvNormal `intersect` equalVersions
      pure $ Suggestion
        { sTarget = extraDep
        , sAction = replaceWith version
        , sDescription = "Same-or-newer version exists on Hackage"
        }

    -- Fall-back for when we can't find Hackage info, so just suggest if there
    -- are newer version-like tags
    suggestGit = do
      version <- headMaybe newerVersions
      pure $ Suggestion
        { sTarget = extraDep
        , sAction = replaceWith version
        , sDescription = "Newer, version-like tag exists"
        }

  suggestHackage <|> suggestGit
