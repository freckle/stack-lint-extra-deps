module Main
  ( main
  ) where

import SLED.Prelude

import Blammo.Logging.Simple
import SLED.App
import SLED.Options
import SLED.Run

main :: IO ()
main = do
  opts@Options {..} <- parseOptions
  app <- App opts <$> newLoggerEnv

  flip runAppT app $ do
    n <- runSLED opts
    logDebug $ "Suggestions found" :# ["count" .= n]

    when (n /= 0 && not oNoExit) $ do
      logDebug "Exiting non-zero (--no-exit to disable)"
      exitFailure
