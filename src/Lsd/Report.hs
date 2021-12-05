module Lsd.Report
  ( Format(..)
  , readFormat
  , formatList
  , getReportSuggestion
  ) where

import RIO

import Lsd.Color
import Lsd.ExtraDep
import Lsd.Suggestion

data Format = Detailed

readFormat :: String -> Either String Format
readFormat = \case
  "detailed" -> Right Detailed
  x -> Left $ "Invalid format: " <> x

formatList :: String
formatList = "detailed"

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
        <> "\n        ⮡ "
        <> sDetails
