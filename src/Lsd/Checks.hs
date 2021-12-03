module Lsd.Checks
  ( ChecksName(..)
  , readChecksName
  , checksNameList
  , checksByName
  ) where

import RIO

import Lsd.Cache
import Lsd.Check
import Lsd.Checks.GitVersion
import Lsd.Checks.HackageVersion
import Lsd.Checks.RedundantGit
import Lsd.Checks.RedundantHackage
import Lsd.StackageResolver
import RIO.Process

data ChecksName
    = AllChecks
    | GitChecks
    | HackageChecks

readChecksName :: String -> Either String ChecksName
readChecksName = \case
  "all" -> Right AllChecks
  "git" -> Right GitChecks
  "hackage" -> Right HackageChecks
  x -> Left $ "Invalid checks name: " <> x

checksNameList :: String
checksNameList = "all, git, hackage"

checksByName
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasLogFunc env
     , HasProcessContext env
     , HasCache env
     )
  => StackageResolver
  -> ChecksName
  -> [Check m]
checksByName resolver = \case
  AllChecks ->
    [ checkRedundantGit
    , checkGitVersion
    , checkRedundantHackage resolver
    , checkHackageVersion resolver
    ]
  GitChecks -> [checkRedundantGit, checkGitVersion]
  HackageChecks ->
    [checkRedundantHackage resolver, checkHackageVersion resolver]
