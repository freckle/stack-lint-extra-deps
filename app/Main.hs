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
  opts <- parseOptions
  logger <- newLoggerEnv
  runAppT (runSLED opts) logger
