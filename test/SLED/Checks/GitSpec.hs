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

spec :: Spec
spec = do
  describe "checkGitVersion" $ do
    it "suggests if there are newer commits" $ do
      let
        gitDep1 =
          GitExtraDep
            { gedRepository = Repository "freckle/foo"
            , gedCommit = CommitSHA "xxxxx"
            }
        gitDep2 =
          GitExtraDep
            { gedRepository = Repository "freckle/foo"
            , gedCommit = CommitSHA "yyyyy"
            }
        mockGit =
          Just
            $ NE.fromList
            $ map
              (,Nothing)
              [ gedCommit gitDep2
              , CommitSHA "abc456"
              , CommitSHA "def456"
              , CommitSHA "jkl789"
              , gedCommit gitDep1
              , CommitSHA "xyz012"
              ]

      suggestions <-
        runTestChecks
          mempty
          mempty
          mockGit
          lts1818
          GitChecks
          (Git gitDep1)

      suggestions
        `shouldBe` [ Suggestion
                      { sTarget = Git gitDep1
                      , sAction = ReplaceWith $ Git gitDep2
                      , sDescription = "There are newer commits (4) on the default branch"
                      }
                   ]

  describe "checkRedundantGit" $ do
    it "suggests if there are newer version-like tags" $ do
      let
        gitDep1 =
          GitExtraDep
            { gedRepository = Repository "freckle/foo"
            , gedCommit = CommitSHA "xxxxx"
            }
        gitDep2 =
          GitExtraDep
            { gedRepository = Repository "freckle/foo"
            , gedCommit = CommitSHA "yyyyy"
            }
        mockGit :: Maybe (NonEmpty (CommitSHA, Maybe Text))
        mockGit =
          Just
            $ NE.fromList
              [ (gedCommit gitDep2, Just "v1.0.2")
              , (CommitSHA "abc456", Nothing)
              , (CommitSHA "def456", Just "beta")
              , (CommitSHA "jkl789", Nothing)
              , (gedCommit gitDep1, Just "v1.0.0")
              ]

      suggestions <-
        runTestChecks
          mempty
          mempty
          mockGit
          lts1818
          GitChecks
          (Git gitDep1)

      suggestions
        `shouldBe` [ Suggestion
                      { sTarget = Git gitDep1
                      , sAction =
                          ReplaceWith
                            $ Hackage
                            $ HackageExtraDep
                              { hedPackage =
                                  PackageName
                                    $ repositoryBaseName
                                    $ gedRepository gitDep1
                              , hedVersion = Just $ unsafeVersion "1.0.2"
                              , hedChecksum = Nothing
                              }
                      , sDescription = "Newer, version-like tag exists"
                      }
                   , Suggestion
                      { sTarget = Git gitDep1
                      , sAction = ReplaceWith $ Git gitDep2
                      , sDescription = "There are newer commits (4) on the default branch"
                      }
                   ]
