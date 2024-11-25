module SLED.Checks
  ( ChecksName (..)
  , checksNameOption
  , checksByName
  ) where

import SLED.Prelude

import Options.Applicative
import SLED.Check
import SLED.Checks.GitVersion
import SLED.Checks.HackageVersion
import SLED.Checks.RedundantGit
import SLED.Checks.RedundantHackage
import SLED.Options.BoundedEnum

data ChecksName
  = NoChecks
  | AllChecks
  | GitChecks
  | HackageChecks
  deriving stock (Bounded, Enum)

instance Semigroup ChecksName where
  NoChecks <> x = x
  x <> NoChecks = x
  AllChecks <> _ = AllChecks
  _ <> AllChecks = AllChecks
  GitChecks <> HackageChecks = AllChecks
  HackageChecks <> GitChecks = AllChecks
  GitChecks <> GitChecks = GitChecks
  HackageChecks <> HackageChecks = HackageChecks

checksNameOption :: Parser ChecksName
checksNameOption =
  boundedEnumOptionWith
    showChecksName
    ( \list ->
        long "checks"
          <> help ("Checks to run, one of: " <> list)
          <> metavar "CHECKS"
    )

showChecksName :: ChecksName -> String
showChecksName = \case
  NoChecks -> "none"
  AllChecks -> "all"
  GitChecks -> "git"
  HackageChecks -> "hackage"

checksByName :: ChecksName -> [Check]
checksByName = \case
  NoChecks -> []
  AllChecks -> checksByName GitChecks <> checksByName HackageChecks
  GitChecks -> [checkRedundantGit, checkGitVersion]
  HackageChecks -> [checkRedundantHackage, checkHackageVersion]
