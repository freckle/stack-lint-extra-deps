module Main
  ( main
  ) where

import SLED.Prelude

import Blammo.Logging.Simple
import SLED.App
import SLED.Options
import SLED.Report
import SLED.Run
import SLED.StackYaml

main :: IO ()
main = do
  opts@Options {..} <- parseOptions
  app <- App opts <$> newLoggerEnv

  flip runAppT app $ do
    logDebug $ "Loading stack.yaml" :# ["path" .= oPath]
    stackYaml <- loadStackYaml oPath
    report <- getReportSuggestion oFormat

    n <- runLsd stackYaml oResolver oChecks oFilter oExcludes report
    logDebug $ "Suggestions found" :# ["count" .= n]

    when (n /= 0 && not oNoExit) $ do
      logDebug "Exiting non-zero (--no-exit to disable)"
      exitFailure
