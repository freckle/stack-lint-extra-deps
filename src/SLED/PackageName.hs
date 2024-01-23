module SLED.PackageName
  ( PackageName (..)
  ) where

import SLED.Prelude

newtype PackageName = PackageName
  { unwrap :: Text
  }
  deriving newtype (Eq, Ord, Show, FromJSON, ToJSON)
