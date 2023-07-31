module SLED.StackageResolver
  ( StackageResolver (..)
  ) where

import SLED.Prelude

newtype StackageResolver = StackageResolver
  { unStackageResolver :: Text
  }
  deriving newtype (Eq, Ord, Show, FromJSON, ToJSON)
