module SLED.Stackage
  ( MonadStackage (..)
  , StackageVersions (..)
  ) where

import SLED.Prelude

import SLED.PackageName
import SLED.StackageResolver
import SLED.Version

class MonadStackage m where
  getStackageVersions
    :: StackageResolver -> PackageName -> m (Maybe StackageVersions)

data StackageVersions = StackageVersions
  { svOnPage :: Version
  , svOnHackage :: Version
  }
