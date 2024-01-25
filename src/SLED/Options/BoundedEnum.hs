module SLED.Options.BoundedEnum
  ( boundedEnumOptionWith
  ) where

import SLED.Prelude

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Options.Applicative

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
