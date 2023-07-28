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
    -- TODO
    -- logDebug $ "Loading " <> fromString oPath
    stackYaml <- loadStackYaml oPath
    report <- getReportSuggestion oFormat

    n <- runLsd opts stackYaml report
    -- TODO
    -- logDebug $ displayShow n <> " suggestion(s) found"

    when (n /= 0 && not oNoExit) $ do
      logDebug "Exiting non-zero (--no-exit to disable)"
      exitFailure
