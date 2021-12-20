module StackYaml
  ( StackYaml(..)
  , loadStackYaml
  ) where

import RIO

import Data.Aeson
import qualified Data.Yaml as Yaml
import ExtraDep
import StackageResolver

data StackYaml = StackYaml
  { syResolver :: StackageResolver
  , syExtraDeps :: [ExtraDep]
  }
  deriving stock Show

instance Display StackYaml where
  display = displayShow

instance FromJSON StackYaml where
  parseJSON = withObject "StackYaml" $ \o -> do
      -- Support stack.yaml or snapshot.yaml syntax
    mExtraDeps <- (<|>) <$> o .:? "extra-deps" <*> o .:? "packages"
    StackYaml <$> o .: "resolver" <*> pure (fromMaybe [] mExtraDeps)

loadStackYaml :: MonadIO m => FilePath -> m StackYaml
loadStackYaml = Yaml.decodeFileThrow
