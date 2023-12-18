module SLED.Display
  ( Display (..)
  , module Blammo.Logging.Colors
  ) where

import SLED.Prelude

import Blammo.Logging.Colors

class Display a where
  display :: Colors -> a -> Text

instance Display Text where
  display _ = id

instance Display a => Display (Marked a) where
  display colors = display colors . markedItem
