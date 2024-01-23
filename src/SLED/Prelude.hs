{-# OPTIONS_GHC -Wno-orphans #-}

module SLED.Prelude
  ( module X
  , module SLED.Prelude
  ) where

import Blammo.Logging as X
import Control.Lens as X (Lens', lens, view)
import Control.Monad.IO.Unlift as X (MonadUnliftIO)
import Data.Aeson as X (FromJSON (..), ToJSON (..))
import Data.Text as X (pack, unpack)
import Data.Traversable as X (for)
import Data.Yaml.Marked as X
import Relude as X

import qualified Data.List.NonEmpty as NE

-- So we can say "Data.Text as X", not "as T"
{-# ANN module ("HLint: ignore Avoid restricted alias" :: String) #-}
{-# ANN module ("HLint: ignore Avoid restricted qualification" :: String) #-}

headMaybe :: [a] -> Maybe a
headMaybe = fmap head . NE.nonEmpty

(<$$>) :: (Functor f0, Functor f1) => (a -> b) -> f0 (f1 a) -> f0 (f1 b)
(<$$>) = fmap . fmap

infixr 8 <$$>

instance ToJSON a => ToJSON (Marked a) where
  toJSON = toJSON . markedItem -- TODO: include location
  toEncoding = toEncoding . markedItem
