module SLED.Report
  ( Format (..)
  , formatOption
  , getReportSuggestion
  ) where

import SLED.Prelude

import Blammo.Logging.Colors
import Blammo.Logging.Logger (pushLoggerLn)
import Options.Applicative
import SLED.ExtraDep
import SLED.Options.BoundedEnum
import SLED.Suggestion

data Format = Detailed
  deriving stock (Bounded, Enum)

formatOption :: Parser Format
formatOption =
  boundedEnumOptionWith
    showFormat
    ( \list ->
        short 'f'
          <> long "format"
          <> help ("Output format, one of: " <> list)
          <> metavar "FORMAT"
          <> value Detailed
    )

showFormat :: Format -> String
showFormat = \case
  Detailed -> "detailed"

getReportSuggestion
  :: (MonadIO m, MonadLogger m, MonadReader env m, HasLogger env)
  => Format
  -> m (Suggestion -> m ())
getReportSuggestion = \case
  Detailed -> do
    Colors {..} <- getColorsLogger
    pure $ \Suggestion {..} ->
      pushLoggerLn
        $ case sAction of
          Remove ->
            green "Remove " <> " " <> magenta (extraDepToText sTarget)
          ReplaceWith r ->
            yellow "Replace"
              <> " "
              <> magenta (extraDepToText sTarget)
              <> " with "
              <> cyan (extraDepToText r)
        <> "\n        ↳ "
        <> sDescription
