module Report
  ( Format(..)
  , formatOption
  , getReportSuggestion
  ) where

import RIO

import Color
import ExtraDep
import Options.Applicative
import Options.BoundedEnum
import Suggestion

data Format = Detailed
  deriving stock (Bounded, Enum)

formatOption :: Parser Format
formatOption = boundedEnumOptionWith
  showFormat
  (\list ->
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
  -> m (ExtraDep -> Suggestion -> m ())
getReportSuggestion = \case
  Detailed -> do
    color <- getColor
    pure $ \extraDep Suggestion {..} ->
      logError
        $ case sAction of
            Remove -> color Green "Remove "
            Replace -> color Yellow "Replace"
        <> " "
        <> color Magenta (display extraDep)
        <> "\n        ↳ "
        <> sDetails
