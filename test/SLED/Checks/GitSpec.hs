{-# LANGUAGE TupleSections #-}

module SLED.Checks.GitSpec
  ( spec
  ) where

import SLED.Prelude

import qualified Data.List.NonEmpty as NE
import SLED.Suggestion
import SLED.Test
import SLED.Version

spec :: Spec
spec = do
  describe "checkGitVersion" $ do
    it "suggests if there are newer commits" $ do
      let
        repo = Repository "freckle/foo"
        commitX = CommitSHA "xxxxx"
        commitY = CommitSHA "yyyyy"
        mcommitX = markAtZero commitX
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
        `shouldReturn` Just (UpdateGitCommit commitY)

  describe "checkRedundantGit" $ do
    it "suggests if there are newer version-like tags" $ do
      let
        repo = Repository "freckle/foo"
        commitX = CommitSHA "xxxxx"
        commitY = CommitSHA "yyyyy"
        mcommitX = markAtZero commitX
        ged = GitExtraDep {repository = repo, commit = mcommitX}
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
        `shouldReturn` Just
          ( ReplaceGitWithHackage
              $ HackageExtraDep
                { package = PackageName "foo"
                , version = parseVersion "1.0.2"
                , checksum = Nothing
                }
          )
