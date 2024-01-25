{-# LANGUAGE TupleSections #-}

module SLED.Checks.GitSpec
  ( spec
  ) where

import SLED.Prelude

import qualified Data.List.NonEmpty as NE
import SLED.Checks
import SLED.GitExtraDep
import SLED.HackageExtraDep
import SLED.Suggestion
import SLED.Test
import SLED.Version

spec :: Spec
spec = do
  let runGitChecks mockGit ged =
        runTestChecks mempty mempty mockGit lts1818 GitChecks
          $ markAtZero (Git ged) "<input>"

  describe "checkGitVersion" $ do
    it "suggests if there are newer commits" $ do
      let
        repo = Repository "freckle/foo"
        commitX = CommitSHA "xxxxx"
        commitY = CommitSHA "yyyyy"
        mcommitX = markAtZero commitX "<input>"
        mockGit =
          Just
            $ NE.fromList
            $ map
              (,Nothing)
              [ commitY
              , CommitSHA "abc456"
              , CommitSHA "def456"
              , CommitSHA "jkl789"
              , commitX
              , CommitSHA "xyz012"
              ]

      runGitChecks mockGit GitExtraDep {repository = repo, commit = mcommitX}
        `shouldReturn` [ Suggestion
                          { action = ReplaceCommit mcommitX commitY
                          , reason = "There are 4 newer commits on the default branch"
                          }
                       ]

  describe "checkRedundantGit" $ do
    it "suggests if there are newer version-like tags" $ do
      let
        repo = Repository "freckle/foo"
        commitX = CommitSHA "xxxxx"
        commitY = CommitSHA "yyyyy"
        mcommitX = markAtZero commitX "<input>"
        ged = GitExtraDep {repository = repo, commit = mcommitX}
        mged = markAtZero ged "<input>"
        mockGit :: Maybe (NonEmpty (CommitSHA, Maybe Text))
        mockGit =
          Just
            $ NE.fromList
              [ (commitY, Just "v1.0.2")
              , (CommitSHA "abc456", Nothing)
              , (CommitSHA "def456", Just "beta")
              , (CommitSHA "jkl789", Nothing)
              , (commitX, Just "v1.0.0")
              ]

      runGitChecks mockGit ged
        `shouldReturn` [ Suggestion
                          { action =
                              ReplaceGitWithHackage mged
                                $ HackageExtraDep
                                  { package = PackageName "foo"
                                  , version = parseVersion "1.0.2"
                                  , checksum = Nothing
                                  }
                          , reason = "Newer, version-like tag exists"
                          }
                       , Suggestion
                          { action = ReplaceCommit mcommitX commitY
                          , reason = "There are 4 newer commits on the default branch"
                          }
                       ]
