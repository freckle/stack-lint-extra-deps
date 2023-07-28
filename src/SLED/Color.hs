module SLED.Color
  ( getColor
  , Color (..)
  ) where

import RIO

data Color
  = Red
  | Green
  | Yellow
  | Cyan
  | Magenta
  | LightGray

getColor
  :: (MonadReader env m, HasLogFunc env)
  => m (Color -> Utf8Builder -> Utf8Builder)
getColor = do
  useColor <- view logFuncUseColorL

  pure
    $ if useColor
      then \color text -> escape color <> text <> reset
      else \_ text -> text

escape :: Color -> Utf8Builder
escape = \case
  Red -> esc "31"
  Green -> esc "32"
  Yellow -> esc "33"
  Cyan -> esc "36"
  Magenta -> esc "95"
  LightGray -> esc "37"

reset :: Utf8Builder
reset = esc "0"

esc :: Utf8Builder -> Utf8Builder
esc x = "\x1b[" <> x <> "m"
