module SLED.Checks.StackageResolver
  ( checkStackageResolver
  ) where

import SLED.Prelude

import SLED.Stackage
import SLED.StackageResolver
import SLED.Suggestion

checkStackageResolver
  :: (Monad m, MonadStackage m)
  => Marked StackageResolver
  -> m (Maybe (Marked (Suggestion StackageResolver)))
checkStackageResolver mresolver = do
  latest <- getLatestInSeries resolver
  let suggestion = (`replaceWith` latest) <$> mresolver
  pure $ suggestion <$ guard (latest > resolver)
 where
  resolver :: StackageResolver
  resolver = markedItem mresolver

replaceWith :: t -> t -> Suggestion t
replaceWith a b =
  Suggestion
    { target = a
    , action = ReplaceWith b
    }
