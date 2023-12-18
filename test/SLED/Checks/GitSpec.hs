{-# LANGUAGE TupleSections #-}

module SLED.Checks.GitSpec
  ( spec
  ) where

import SLED.Prelude

import qualified Data.List.NonEmpty as NE
import SLED.Checks
import SLED.GitExtraDep
import SLED.Suggestion
import SLED.Test

spec :: Spec
spec = do
  describe "checkGitVersion" $ do
    it "suggests if there are newer commits" $ do
      let
        gitDep1 =
          GitExtraDep
            { gedRepository = Repository "freckle/foo"
            , gedCommit = markAtZero (CommitSHA "xxxxx") "<input>"
            }
        gitDep2 =
          GitExtraDep
            { gedRepository = Repository "freckle/foo"
            , gedCommit = markAtZero (CommitSHA "yyyyy") "<input>"
            }
        extraDep = markAtZero (Git gitDep1) "<input>"

        mockGit =
          Just
            $ NE.fromList
            $ map
              (,Nothing)
              [ markedItem $ gedCommit gitDep2
              , CommitSHA "abc456"
              , CommitSHA "def456"
              , CommitSHA "jkl789"
              , markedItem $ gedCommit gitDep1
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
                      { sTarget = extraDep
                      , sAction = replaceGitExtraDepCommit gitDep1 (CommitSHA "yyyyy")
                      , sDescription = "There are newer commits (4) on the default branch"
                      }
                   ]

  describe "checkRedundantGit" $ do
    it "suggests if there are newer version-like tags" $ do
      let
        gitDep1 =
          GitExtraDep
            { gedRepository = Repository "freckle/foo"
            , gedCommit = markAtZero (CommitSHA "xxxxx") "<input>"
            }
        gitDep2 =
          GitExtraDep
            { gedRepository = Repository "freckle/foo"
            , gedCommit = markAtZero (CommitSHA "yyyyy") "<input>"
            }
        extraDep = markAtZero (Git gitDep1) "<input>"

        mockGit :: Maybe (NonEmpty (CommitSHA, Maybe Text))
        mockGit =
          Just
            $ NE.fromList
              [ (markedItem $ gedCommit gitDep2, Just "v1.0.2")
              , (CommitSHA "abc456", Nothing)
              , (CommitSHA "def456", Just "beta")
              , (CommitSHA "jkl789", Nothing)
              , (markedItem $ gedCommit gitDep1, Just "v1.0.0")
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
                      { sTarget = extraDep
                      , sAction = replaceGitExtraDep (gitDep1 <$ extraDep) $ unsafeVersion "1.0.2"
                      , sDescription = "Newer, version-like tag exists"
                      }
                   , Suggestion
                      { sTarget = extraDep
                      , sAction = replaceGitExtraDepCommit gitDep1 (CommitSHA "yyyyy")
                      , sDescription = "There are newer commits (4) on the default branch"
                      }
                   ]
