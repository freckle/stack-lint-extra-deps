module SLED.StackYaml
  ( MonadStackYaml (..)
  , StackYaml (..)
  ) where

import SLED.Prelude

import Data.Aeson
import SLED.ExtraDep
import SLED.StackageResolver

class MonadStackYaml m where
  loadStackYaml :: FilePath -> m StackYaml

data StackYaml = StackYaml
  { syResolver :: StackageResolver
  , syExtraDeps :: [ExtraDep]
  }
  deriving stock (Show)

instance ToJSON StackYaml where
  toJSON = toJSON . show @Text
  toEncoding = toEncoding . show @Text

instance FromJSON StackYaml where
  parseJSON = withObject "StackYaml" $ \o -> do
    -- Support stack.yaml or snapshot.yaml syntax
    mExtraDeps <- (<|>) <$> o .:? "extra-deps" <*> o .:? "packages"
    StackYaml <$> o .: "resolver" <*> pure (fromMaybe [] mExtraDeps)
