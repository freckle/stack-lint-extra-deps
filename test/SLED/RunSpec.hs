module SLED.RunSpec
  ( spec
  ) where

import SLED.Test

import SLED.ExtraDep
import SLED.GitExtraDep
import SLED.HackageExtraDep
import SLED.Run (shouldIncludeExtraDep)

spec :: Spec
spec = do
  it "works for our lts-18.18 example" $ do
    runTestAppM
      $ withMocks
        ( hackage "faktory" "1.1.3.0"
            . hackage "freckle-app" "1.21.0.0"
            . hackage "hspec" "2.11.10"
            . hackage "hspec-core" "2.11.10"
            . hackage "hspec-junit-formatter" "1.1.2.1"
            . hackage "network" "3.2.7"
            . stackage "lts-22.43" "dhall" "1.40.0"
            . stackage "lts-22.43" "generic-lens" "2.0.0.1"
            . stackageResolvers ["nightly-2024-12-03", "nightly-2024-11-01"]
            . stackageResolvers ["lts-22.43", "lts-22.32", "lts-21.20"]
            . git "https://github.com/freckle/asana" []
            . git
              "https://github.com/freckle/yesod-routes-flow"
              [ ("2a9cd873880956dd9a0999b593022d3c74xxxxxx", Nothing)
              , ("2a9cd873880956dd9a0999b593022d3c74yyyyyy", Just "v3.0.0.2")
              , ("2a9cd873880956dd9a0999b593022d3c74zzzzzz", Nothing)
              , ("2a9cd873880956dd9a0999b593022d3c746324e8", Just "v0.0")
              , ("2a9cd873880956dd9a0999b593022d3c74aaaaaa", Nothing)
              , ("2a9cd873880956dd9a0999b593022d3c74bbbbbb", Nothing)
              , ("2a9cd873880956dd9a0999b593022d3c74cccccc", Nothing)
              ]
        )
      $ assertAutoFixed
        [ "-resolver: lts-18.18"
        , "+resolver: lts-22.43"
        , ""
        , " extra-deps:"
        , "-  - dhall-1.37.1"
        , "-  - faktory-1.1.2.0"
        , "-  - freckle-app-1.0.1.1"
        , "-  - generic-lens-2.0.0.0"
        , "-  - hspec-2.8.3"
        , "-  - hspec-core-2.8.3"
        , "-  - hspec-junit-formatter-1.0.1.0"
        , "-  - network-3.1.2.5"
        , "+  - faktory-1.1.3.0"
        , "+  - freckle-app-1.21.0.0"
        , "+  - hspec-2.11.10"
        , "+  - hspec-core-2.11.10"
        , "+  - hspec-junit-formatter-1.1.2.1"
        , "+  - network-3.2.7"
        , ""
        , "   - git: https://github.com/freckle/asana"
        , "     commit: 91ce6ade118674bb273966943370684eba71f227"
        , ""
        , "-  - github: freckle/yesod-routes-flow"
        , "-    commit: 2a9cd873880956dd9a0999b593022d3c746324e8"
        , "+  - yesod-routes-flow-3.0.0.2"
        ]

  describe "shouldIncludeExtraDep" $ do
    context "Hackage extra deps" $ do
      it "is True by default" $ do
        Hackage freckleApp1011
          `shouldSatisfy` shouldIncludeExtraDep Nothing []

      it "is True if matching given include" $ do
        Hackage freckleApp1011
          `shouldSatisfy` shouldIncludeExtraDep (Just "*-app") []

      it "is True if not matching any given excludes" $ do
        Hackage freckleApp1011
          `shouldSatisfy` shouldIncludeExtraDep Nothing ["other-package"]

      it "is False if not matching a given include" $ do
        Hackage freckleApp1011
          `shouldNotSatisfy` shouldIncludeExtraDep (Just "other-package") []

      it "is False is matching any given excludes" $ do
        Hackage freckleApp1011
          `shouldNotSatisfy` shouldIncludeExtraDep Nothing ["freckle-*"]

    context "Git extra deps" $ do
      it "is True by default" $ do
        Git yesodFlowRoutesGitHub
          `shouldSatisfy` shouldIncludeExtraDep Nothing []

      it "is True if org/repo matches given include" $ do
        Git yesodFlowRoutesGitHub
          `shouldSatisfy` shouldIncludeExtraDep (Just "freckle/*") []

      it "is True if not matching any given excludes" $ do
        Git yesodFlowRoutesGitHub
          `shouldSatisfy` shouldIncludeExtraDep Nothing ["other-package"]

      it "is False is not matching a given include" $ do
        Git yesodFlowRoutesGitHub
          `shouldNotSatisfy` shouldIncludeExtraDep (Just "other-package") []

      it "is False is matching any given excludes" $ do
        Git yesodFlowRoutesGitHub
          `shouldNotSatisfy` shouldIncludeExtraDep Nothing ["*/yesod-*"]

freckleApp1011 :: HackageExtraDep
freckleApp1011 =
  HackageExtraDep
    { package = "freckle-app"
    , version = "1.0.1.1"
    }

yesodFlowRoutesGitHub :: GitExtraDep
yesodFlowRoutesGitHub =
  GitExtraDep
    { repository = "https://github.com/freckle/yesod-routes-flow"
    , commit = "2a9cd873880956dd9a0999b593022d3c746324e8"
    }
