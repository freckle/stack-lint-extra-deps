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
  describe "checkGitVersion" $ do
    it "suggests if there are newer commits" $ do
      let
        gitDep1 =
          GitExtraDep
            { repository = Repository "freckle/foo"
            , commit = markAtZero (CommitSHA "xxxxx") "<input>"
            }
        gitDep2 =
          GitExtraDep
            { repository = Repository "freckle/foo"
            , commit = markAtZero (CommitSHA "yyyyy") "<input>"
            }
        extraDep = markAtZero (Git gitDep1) "<input>"

        mockGit =
          Just
            $ NE.fromList
            $ map
              (,Nothing)
              [ markedItem gitDep2.commit
              , CommitSHA "abc456"
              , CommitSHA "def456"
              , CommitSHA "jkl789"
              , markedItem gitDep1.commit
              , CommitSHA "xyz012"
              ]

      suggestions <-
        runTestChecks
          mempty
          mempty
          mockGit
          lts1818
          GitChecks
          extraDep

      suggestions
        `shouldBe` [ Suggestion
                      { action =
                          ReplaceCommit (markAtZero (CommitSHA "xxxxx") "<input>") (CommitSHA "yyyyy")
                      , reason = "There are 4 newer commits on the default branch"
                      }
                   ]

  describe "checkRedundantGit" $ do
    it "suggests if there are newer version-like tags" $ do
      let
        gitDep1 =
          GitExtraDep
            { repository = Repository "freckle/foo"
            , commit = markAtZero (CommitSHA "xxxxx") "<input>"
            }
        gitDep2 =
          GitExtraDep
            { repository = Repository "freckle/foo"
            , commit = markAtZero (CommitSHA "yyyyy") "<input>"
            }
        extraDep = markAtZero (Git gitDep1) "<input>"

        mockGit :: Maybe (NonEmpty (CommitSHA, Maybe Text))
        mockGit =
          Just
            $ NE.fromList
              [ (markedItem gitDep2.commit, Just "v1.0.2")
              , (CommitSHA "abc456", Nothing)
              , (CommitSHA "def456", Just "beta")
              , (CommitSHA "jkl789", Nothing)
              , (markedItem gitDep1.commit, Just "v1.0.0")
              ]

      suggestions <-
        runTestChecks
          mempty
          mempty
          mockGit
          lts1818
          GitChecks
          extraDep

      suggestions
        `shouldBe` [ Suggestion
                      { action =
                          ReplaceGitWithHackage (gitDep1 <$ extraDep)
                            $ HackageExtraDep
                              { package = PackageName "foo"
                              , version = parseVersion "1.0.2"
                              , checksum = Nothing
                              }
                      , reason = "Newer, version-like tag exists"
                      }
                   , Suggestion
                      { action =
                          ReplaceCommit (markAtZero (CommitSHA "xxxxx") "<input>") (CommitSHA "yyyyy")
                      , reason = "There are 4 newer commits on the default branch"
                      }
                   ]
