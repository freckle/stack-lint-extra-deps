module SLED.Checks.GitSpec
  ( spec
  ) where

import SLED.Test

spec :: Spec
spec = do
  describe "checkGitVersion" $ do
    it "suggests if there are newer commits" $ do
      runTestAppM
        $ withGitClone
          "https://github.com/freckle/foo"
          [ ("yyyyyy", Nothing)
          , ("abc456", Nothing)
          , ("def456", Nothing)
          , ("jkl789", Nothing)
          , ("xxxxxx", Nothing)
          , ("xyz012", Nothing)
          ]
        $ assertAutoFixed
          [ " resolver: lts-0.0"
          , " extra-deps:"
          , "   - github: freckle/foo"
          , "-    commit: xxxxxx"
          , "+    commit: yyyyyy"
          ]

  describe "checkRedundantGit" $ do
    it "suggests if there are newer version-like tags" $ do
      runTestAppM
        $ withGitClone
          "git@github.com:freckle/foo"
          [ ("yyyyyy", Just "v1.0.2")
          , ("abc456", Nothing)
          , ("def456", Just "beta")
          , ("jkl789", Nothing)
          , ("xxxxxx", Just "v1.0.0")
          ]
        $ assertAutoFixed
          [ " resolver: lts-0.0"
          , " extra-deps:"
          , "-  - git: git@github.com:freckle/foo"
          , "-    commit: xxxxxx"
          , "+  - foo-1.0.2"
          ]
