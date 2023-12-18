module SLED.PackageName
  ( PackageName (..)
  ) where

import SLED.Prelude

import SLED.Display

newtype PackageName = PackageName
  { unPackageName :: Text
  }
  deriving newtype (Eq, Ord, Show, Display, FromJSON, ToJSON)
