module SLED.Suggestion.Format.Target
  ( IsTarget (..)
  , replaceMarkedTarget
  ) where

import SLED.Prelude

import qualified Data.Text as T
import Data.Yaml.Marked.Replace
import SLED.ExtraDep
import SLED.GitExtraDep
import SLED.HackageExtraDep
import SLED.PackageName
import SLED.StackageResolver
import SLED.Suggestion
import SLED.Version

class IsTarget a where
  formatTarget :: a -> Text

  -- | Allows marking parts of a target for specific actions
  --
  -- We return void since we don't know what type that part could be, and
  -- at the point we use this we only care about the marks anyway.
  getTargetMark :: Marked (Suggestion a) -> Marked ()
  getTargetMark m = void m

instance IsTarget ExtraDep where
  formatTarget = \case
    Hackage hed -> formatTarget hed
    Git ged ->
      repositoryBase ged.repository
        <> "@"
        <> T.take 7 (formatTarget $ markedItem ged.commit)
    Other {} -> "<unknown>"

  getTargetMark m = case (s.target, s.action) of
    (Hackage hed, UpdateHackageVersion {}) -> void hed.version
    (Git ged, UpdateGitCommit {}) -> void ged.commit
    _ -> void m
   where
    s = markedItem m

instance IsTarget HackageExtraDep where
  formatTarget hed =
    hed.package.unwrap <> "-" <> pack (showVersion $ markedItem hed.version)

instance IsTarget CommitSHA where
  formatTarget = (.unwrap)

instance IsTarget Version where
  formatTarget = pack . showVersion

instance IsTarget StackageResolver where
  formatTarget = stackageResolverToText

replaceMarkedTarget
  :: (IsTarget a, IsTarget b)
  => Marked (Suggestion a)
  -> b
  -> Replace
replaceMarkedTarget a =
  replaceMarked (getTargetMark a) . encodeUtf8 . formatTarget
