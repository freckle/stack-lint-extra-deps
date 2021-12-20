module Checks
  ( ChecksName(..)
  , checksNameOption
  , checksByName
  ) where

import RIO

import Check
import Checks.GitVersion
import Checks.HackageVersion
import Checks.RedundantGit
import Checks.RedundantHackage
import Options.Applicative
import Options.BoundedEnum

data ChecksName
    = AllChecks
    | GitChecks
    | HackageChecks
    deriving stock (Bounded, Enum)

checksNameOption :: Parser ChecksName
checksNameOption = boundedEnumOptionWith
  showChecksName
  (\list ->
    long "checks"
      <> help ("Checks to run, one of: " <> list)
      <> metavar "CHECKS"
      <> value AllChecks
  )

showChecksName :: ChecksName -> String
showChecksName = \case
  AllChecks -> "all"
  GitChecks -> "git"
  HackageChecks -> "hackage"

checksByName :: ChecksName -> [Check]
checksByName = \case
  AllChecks -> checksByName GitChecks <> checksByName HackageChecks
  GitChecks -> [checkRedundantGit, checkGitVersion]
  HackageChecks -> [checkRedundantHackage, checkHackageVersion]
