module SLED.PackageName
  ( PackageName (..)
  ) where

import SLED.Prelude

newtype PackageName = PackageName
  { unPackageName :: Text
  }
  deriving newtype (Show, FromJSON, ToJSON)
