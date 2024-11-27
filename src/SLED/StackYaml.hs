{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SLED.StackYaml
  ( StackYaml (..)
  , setStackYamlResolver
  , decodeStackYaml
  ) where

import SLED.Prelude

import Data.Yaml.Marked.Parse
import Data.Yaml.Marked.Value
import SLED.ExtraDep
import SLED.StackageResolver

data StackYaml = StackYaml
  { resolver :: Marked StackageResolver
  , extraDeps :: [Marked ExtraDep]
  }
  deriving stock (Show)

instance ToJSON StackYaml where
  toJSON = toJSON . show @Text
  toEncoding = toEncoding . show @Text

setStackYamlResolver :: StackageResolver -> StackYaml -> StackYaml
setStackYamlResolver r stackYaml = stackYaml {resolver = r <$ stackYaml.resolver}

decodeStackYaml :: Marked Value -> Either String (Marked StackYaml)
decodeStackYaml = withObject "StackYaml" $ \o -> do
  -- Support stack.yaml or snapshot.yaml syntax
  mExtraDeps <- (<|>) <$> o .:? "extra-deps" <*> o .:? "packages"
  StackYaml
    <$> (json =<< o .: "resolver")
    <*> maybe (pure []) (fmap markedItem . array decodeExtraDep) mExtraDeps
