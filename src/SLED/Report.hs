module SLED.Report
  ( Format (..)
  , formatOption
  , getReportSuggestion
  ) where

import SLED.Prelude

import Options.Applicative
import SLED.Color
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
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => Format
  -> m (Suggestion -> m ())
getReportSuggestion = \case
  Detailed -> do
    color <- getColor
    pure $ \Suggestion {..} ->
      logError
        $ case sAction of
          Remove ->
            color Green "Remove " <> " " <> color Magenta (display sTarget)
          ReplaceWith r ->
            color Yellow "Replace"
              <> " "
              <> color Magenta (display sTarget)
              <> " with "
              <> color Cyan (display r)
        <> "\n        ↳ "
        <> sDescription
