module SLED.HackageExtraDep
  ( HackageExtraDep (..)
  , hackageExtraDepFromText
  ) where

import SLED.Prelude

import Data.Aeson
import Data.List (elemIndices)
import qualified Data.Text as T
import SLED.PackageName
import SLED.Version

data HackageExtraDep = HackageExtraDep
  { package :: PackageName
  , version :: Maybe Version
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON HackageExtraDep where
  parseJSON =
    withText "HackageExtraDep" $ either fail pure . hackageExtraDepFromText

hackageExtraDepFromText :: Text -> Either String HackageExtraDep
hackageExtraDepFromText x =
  Right
    HackageExtraDep
      { package = PackageName package
      , version = mVersion
      }
 where
  (package, mVersion) = splitPackageVersion x

-- |
--
-- >>> second (fmap showVersion) $ splitPackageVersion "optparse-applicative-0.15.1.0@rev:1"
-- ("optparse-applicative",Just "0.15.1.0@rev:1")
splitPackageVersion :: Text -> (Text, Maybe Version)
splitPackageVersion x =
  fromMaybe (x, Nothing)
    $ headMaybe
    $ filter (isJust . snd)
    $ map (second (parseVersion . unpack))
    $ breaksOn '-' x

-- |
--
-- >>> breaksOn '-' "oidc-client-0.5.0.0"
-- [("oidc","client-0.5.0.0"),("oidc-client","0.5.0.0")]
breaksOn :: Char -> Text -> [(Text, Text)]
breaksOn c t =
  map (bimap pack (T.drop 1 . pack) . (`splitAt` s))
    $ elemIndices c s
 where
  s = unpack t
