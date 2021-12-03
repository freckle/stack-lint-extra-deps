module Main
    ( main
    ) where

import Prelude

import Data.Aeson
import qualified Data.Yaml as Yaml
import Test.DocTest

newtype Package = Package [String]

instance FromJSON Package where
    parseJSON =
        withObject "Package" $ \o -> Package <$> o .: "default-extensions"

main :: IO ()
main = do
    Package extensions <- Yaml.decodeFileThrow "package.yaml"
    doctest $ ["src"] <> map ("-X" <>) extensions
