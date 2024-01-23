

module SLED.Suggestion
  ( Suggestion (..)
  , SuggestionAction (..)
  , replaceGitExtraDep
  , replaceGitExtraDepCommit
  , replaceHackageDep
  ) where

import SLED.Prelude

import SLED.ExtraDep
import SLED.GitExtraDep
import SLED.HackageExtraDep
import SLED.PackageName
import SLED.Version

data SuggestionAction
  = Remove
  | ReplaceWith ExtraDep
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data Suggestion = Suggestion
  { sTarget :: Marked ExtraDep
  , sAction :: SuggestionAction
  , sDescription :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- instance Display Suggestion where
--   display colors Suggestion {..} =
--     displayAction colors sTarget sAction
--       <> " ("
--       <> displayMarks sTarget
--       <> ")"
--       <> "\n        ↳ "
--       <> sDescription

-- displayAction :: Colors -> Marked ExtraDep -> SuggestionAction -> Text
-- displayAction colors@Colors {..} (markedItem -> x) = \case
--   Remove ->
--     green "Remove"
--       <> "  "
--       <> magenta (display colors x)
--   ReplaceWith y ->
--     yellow "Replace"
--       <> " "
--       <> magenta (display colors x)
--       <> " with "
--       <> cyan (display colors y)

-- displayMarks :: Marked a -> Text
-- displayMarks m =
--   pack
--     $ mconcat
--       [ markedPath m
--       , ":" <> show (locationLine $ markedLocationStart m)
--       , ":" <> show (locationLine $ markedLocationEnd m)
--       ]

replaceGitExtraDep :: Marked GitExtraDep -> Version -> SuggestionAction
replaceGitExtraDep mged version =
  ReplaceWith
    $ Hackage
    $ HackageExtraDep
      { hedPackage = PackageName $ repositoryBaseName $ gedRepository $ markedItem mged
      , hedVersion = Just version
      , hedChecksum = Nothing
      }

replaceGitExtraDepCommit :: GitExtraDep -> CommitSHA -> SuggestionAction
replaceGitExtraDepCommit ged sha =
  ReplaceWith $ Git $ ged {gedCommit = sha <$ gedCommit ged}

replaceHackageDep :: Marked HackageExtraDep -> Version -> SuggestionAction
replaceHackageDep mhed version =
  ReplaceWith $ Hackage $ (markedItem mhed) {hedVersion = Just version}
