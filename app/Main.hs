module Main
  ( main
  ) where

import SLED.Prelude

import Blammo.Logging.Simple
import Data.Version (showVersion)
import Paths_stack_lint_extra_deps (version)
import SLED.App
import SLED.Options.Parse
import SLED.Run

main :: IO ()
main =
  parseOptions >>= \case
    Left PrintVersion ->
      putStrLn $ "stack-lint-extra-deps-" <> showVersion version
    Right opts -> withLoggerEnv $ runAppT $ runSLED opts
