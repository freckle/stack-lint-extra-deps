module SLED.StackageResolver
  ( StackageResolver
  , stackageResolver
  , unStackageResolver
  ) where

import SLED.Prelude

newtype StackageResolver = StackageResolver Text
  deriving newtype (Show, FromJSON, ToJSON)

stackageResolver :: String -> Either String StackageResolver
stackageResolver = Right . StackageResolver . pack

unStackageResolver :: StackageResolver -> Text
unStackageResolver (StackageResolver x) = x
