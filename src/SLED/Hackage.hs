module SLED.Hackage
  ( MonadHackage (..)
  , HackageVersions (..)
  ) where

import SLED.Prelude

import Data.Aeson
import SLED.PackageName
import SLED.Version

class MonadHackage m where
  getHackageVersions :: PackageName -> m (Maybe HackageVersions)

-- | <https://hackage.haskell.org/api#versions>
data HackageVersions = HackageVersions
  { hvNormal :: [Version]
  , hvUnpreferred :: [Version]
  , hvDeprecated :: [Version]
  }
  deriving stock (Show)

instance FromJSON HackageVersions where
  parseJSON = withObject "HackageVersions" $ \o ->
    HackageVersions
      <$> (o .:? "normal-version" .!= [])
      <*> (o .:? "unpreferred-version" .!= [])
      <*> (o .:? "deprecated-version" .!= [])
