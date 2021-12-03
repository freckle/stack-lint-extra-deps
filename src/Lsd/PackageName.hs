module Lsd.PackageName
  ( PackageName
  , packageName
  , unPackageName
  ) where

import RIO

import Data.Aeson

newtype PackageName = PackageName Text
    deriving newtype (Show, Display, FromJSON)

packageName :: Text -> PackageName
packageName = PackageName

unPackageName :: PackageName -> Text
unPackageName (PackageName x) = x
