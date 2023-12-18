module SLED.StackYaml
  ( StackYaml (..)
  , decodeStackYaml
  ) where

import SLED.Prelude

import Data.Yaml.Marked.Parse
import Data.Yaml.Marked.Value
import SLED.ExtraDep
import SLED.StackageResolver

data StackYaml = StackYaml
  { syResolver :: Marked StackageResolver
  , syExtraDeps :: [Marked ExtraDep]
  }
  deriving stock (Show)

instance ToJSON StackYaml where
  toJSON = toJSON . show @Text
  toEncoding = toEncoding . show @Text

decodeStackYaml :: Marked Value -> Either String (Marked StackYaml)
decodeStackYaml = withObject "StackYaml" $ \o -> do
  -- Support stack.yaml or snapshot.yaml syntax
  mExtraDeps <- (<|>) <$> o .:? "extra-deps" <*> o .:? "packages"
  StackYaml
    <$> (json =<< o .: "resolver")
    <*> maybe (pure []) (fmap markedItem . array decodeExtraDep) mExtraDeps
