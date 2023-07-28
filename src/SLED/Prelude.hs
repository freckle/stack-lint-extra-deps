module SLED.Prelude
  ( module X
  , module SLED.Prelude
  ) where

import Blammo.Logging as X
import Control.Lens as X (Lens', lens, view)
import Control.Monad.IO.Unlift as X (MonadUnliftIO)
import Data.Aeson as X (FromJSON (..), ToJSON (..), Value (..))
import Data.Text as X (pack, unpack)
import Data.Traversable as X (for)
import Relude as X

import qualified Data.List.NonEmpty as NE

-- So we can say "Data.Text as X", not "as T"
{-# ANN module ("HLint: ignore Avoid restricted alias" :: String) #-}
{-# ANN module ("HLint: ignore Avoid restricted qualification" :: String) #-}

headMaybe :: [a] -> Maybe a
headMaybe = fmap head . NE.nonEmpty
