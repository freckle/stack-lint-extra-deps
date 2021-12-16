module Lsd.RunSpec
  ( spec
  ) where

import RIO

import Lsd.App
import Lsd.Checks
import Lsd.Options
import Lsd.Report
import Lsd.Run
import Lsd.StackYaml
import Test.Hspec (Spec, describe, example, it)
import Test.Hspec.Expectations.Lifted

spec :: Spec
spec = do
  describe "runLsd" $ do
    it "finds 9 Hackage suggestions in the lts-18.18 example" $ example $ do
      let opts = testExampleOptions "lts-18.18" HackageChecks
      stackYaml <- loadStackYaml $ oPath opts
      withApp opts $ \app -> runRIO app $ do
        n <- runLsd opts stackYaml $ \_ _ -> pure ()
        n `shouldBe` 9

testExampleOptions :: String -> ChecksName -> Options
testExampleOptions name checks = Options
  { oResolver = Nothing
  , oExcludes = []
  , oChecks = checks
  , oFormat = Detailed
  , oNoExit = True
  , oColor = ColorNever
  , oVerbose = False
  , oPath = "test/examples/" <> name <> ".yaml"
  }
