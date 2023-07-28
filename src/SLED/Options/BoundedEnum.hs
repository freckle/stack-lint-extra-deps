module SLED.Options.BoundedEnum
  ( boundedEnumOption
  , boundedEnumOptionWith
  ) where

import SLED.Prelude

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Options.Applicative

boundedEnumOption
  :: (Bounded a, Enum a, Show a) => (String -> Mod OptionFields a) -> Parser a
boundedEnumOption = boundedEnumOptionWith show

boundedEnumOptionWith
  :: (Bounded a, Enum a)
  => (a -> String)
  -> (String -> Mod OptionFields a)
  -> Parser a
boundedEnumOptionWith show' toMod =
  option (eitherReader $ readEnum show')
    $ toMod (listEnum show')
    <> showDefaultWith show'

readEnum :: (Bounded a, Enum a) => (a -> String) -> String -> Either String a
readEnum show' x =
  maybe (Left $ "Invalid option: " <> x) Right
    $ Map.lookup x
    $ Map.fromList
    $ map (show' &&& id) [minBound .. maxBound]

listEnum :: (Bounded a, Enum a) => (a -> String) -> String
listEnum show' =
  unpack $ T.intercalate ", " $ map (pack . show') [minBound .. maxBound]

-- readColorOption :: String -> Either String ColorOption
-- readColorOption = \case
--   "auto" -> Right ColorAuto
--   "always" -> Right ColorAlways
--   "never" -> Right ColorNever
--   x -> Left $ "Invalid color option: " <> x

-- colorOptionList :: String
-- colorOptionList = "auto, always, never"

-- showColorOption :: ColorOption -> String
-- showColorOption = \case
--   ColorAuto -> "auto"
--   ColorAlways -> "always"
--   ColorNever -> "never"
