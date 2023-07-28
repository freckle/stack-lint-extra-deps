module SLED.RunSpec
  ( spec
  ) where

import SLED.Prelude

import SLED.Checks
import SLED.Run
import SLED.StackYaml
import SLED.Test

spec :: Spec
spec = do
  describe "runLsd" $ do
    it "finds 10 Hackage suggestions in the lts-18.18 example" $ example $ do
      stackYaml <- loadStackYaml "test/examples/lts-18.18.yaml"
      testApp <- newTestApp stackYaml

      flip runTestAppT testApp $ do
        n <- runLsd stackYaml Nothing HackageChecks Nothing [] $ \_ -> pure ()
        n `shouldBe` 10
