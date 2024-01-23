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
  { target :: Marked ExtraDep
  , action :: SuggestionAction
  , description :: Text
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
      { package = PackageName $ repositoryBaseName $ (.repository) $ markedItem mged
      , version = Just version
      , checksum = Nothing
      }

replaceGitExtraDepCommit :: GitExtraDep -> CommitSHA -> SuggestionAction
replaceGitExtraDepCommit ged sha =
  ReplaceWith $ Git $ ged {commit = sha <$ ged.commit}

replaceHackageDep :: Marked HackageExtraDep -> Version -> SuggestionAction
replaceHackageDep mhed version =
  ReplaceWith $ Hackage $ (markedItem mhed) {version = Just version}
