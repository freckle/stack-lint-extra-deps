module SLED.PackageName
  ( PackageName
  , packageName
  , unPackageName
  ) where

import SLED.Prelude

newtype PackageName = PackageName Text
  deriving newtype (Show, FromJSON, ToJSON)

packageName :: Text -> PackageName
packageName = PackageName

unPackageName :: PackageName -> Text
unPackageName (PackageName x) = x
