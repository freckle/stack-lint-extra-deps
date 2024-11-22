module SLED.StackageResolver
  ( StackageResolver
  , stackageResolverSameSeries
  , stackageResolverToText
  , stackageResolverFromText
  , readStackageResolver
  ) where

import SLED.Prelude

import Data.Aeson (withText)
import SLED.Parse

data StackageResolver
  = Nightly Text
  | LTS Nat Nat
  | Other Text
  deriving stock (Eq, Ord, Show)

instance FromJSON StackageResolver where
  parseJSON = withText "StackageResolver" $ pure . stackageResolverFromText

instance ToJSON StackageResolver where
  toJSON = toJSON . stackageResolverToText
  toEncoding = toEncoding . stackageResolverToText

stackageResolverSameSeries :: StackageResolver -> StackageResolver -> Bool
stackageResolverSameSeries =
  curry $ \case
    (Nightly {}, Nightly {}) -> True
    (LTS {}, LTS {}) -> True
    _ -> False

stackageResolverToText :: StackageResolver -> Text
stackageResolverToText = \case
  Nightly x -> "nightly-" <> x
  LTS major minor -> "lts-" <> pack (show major) <> "." <> pack (show minor)
  Other x -> x

stackageResolverFromText :: Text -> StackageResolver
stackageResolverFromText x = parseOr (parseNightly <|> parseLTS) (Other x) x

parseNightly :: ReadP StackageResolver
parseNightly = do
  suffix <- string "nightly-" *> many1 anyChar
  pure $ Nightly $ pack suffix

parseLTS :: ReadP StackageResolver
parseLTS =
  LTS
    <$> (string "lts-" *> nat)
    <*> (char '.' *> nat)

readStackageResolver :: String -> StackageResolver
readStackageResolver = stackageResolverFromText . pack
