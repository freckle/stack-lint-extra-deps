module SLED.StackageResolver
  ( StackageResolver
  , stackageResolver
  , unStackageResolver
  ) where

import SLED.Prelude

import Data.Aeson

newtype StackageResolver = StackageResolver Text
  deriving newtype (Show, Display, FromJSON)

stackageResolver :: String -> Either String StackageResolver
stackageResolver = Right . StackageResolver . pack

unStackageResolver :: StackageResolver -> Text
unStackageResolver (StackageResolver x) = x
