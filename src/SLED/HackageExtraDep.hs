module SLED.HackageExtraDep
  ( HackageExtraDep (..)
  , SHA256 (..)
  , hackageExtraDepFromText
  ) where

import SLED.Prelude

import Data.Aeson
import Data.List (elemIndices)
import qualified Data.Text as T
import SLED.PackageName
import SLED.Version

data HackageExtraDep = HackageExtraDep
  { hedPackage :: PackageName
  , hedVersion :: Maybe Version
  , hedChecksum :: Maybe SHA256
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON HackageExtraDep where
  parseJSON =
    withText "HackageExtraDep" $ either fail pure . hackageExtraDepFromText

newtype SHA256 = SHA256 Text
  deriving newtype (Eq, Show, FromJSON, ToJSON)

hackageExtraDepFromText :: Text -> Either String HackageExtraDep
hackageExtraDepFromText x =
  Right
    HackageExtraDep
      { hedPackage = PackageName package
      , hedVersion = mVersion
      , hedChecksum = do
          guard $ not $ T.null suffix
          pure $ SHA256 $ T.drop 1 suffix
      }
 where
  (prefix, suffix) = T.breakOn "@" x
  (package, mVersion) = splitPackageVersion prefix

-- |
--
-- >>> second (fmap showVersion) $ splitPackageVersion "optparse-applicative-0.15.1.0"
-- ("optparse-applicative",Just "0.15.1.0")
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
