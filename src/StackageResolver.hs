module StackageResolver
  ( StackageResolver
  , stackageResolver
  , unStackageResolver
  ) where

import RIO

import Data.Aeson
import RIO.Text (pack)

newtype StackageResolver = StackageResolver Text
    deriving newtype (Show, Display, FromJSON)

stackageResolver :: String -> Either String StackageResolver
stackageResolver = Right . StackageResolver . pack

unStackageResolver :: StackageResolver -> Text
unStackageResolver (StackageResolver x) = x
