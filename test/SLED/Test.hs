{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SLED.Test
  ( TestAppM
  , runTestAppM
  , assertAutoFixed
  , assertNoFixes

    -- * Re-exports
  , module SLED.Prelude
  , module SLED.Test.Mocks
  , module Test.Hspec
  ) where

import SLED.Prelude

import Blammo.Logging.LogSettings (defaultLogSettings)
import Blammo.Logging.Logger (newTestLogger)
import Blammo.Logging.Setup
import Control.Lens ((^.))
import qualified Data.Text as T
import SLED.Checks (ChecksName (..))
import SLED.Context (contentsL, execContextT)
import SLED.GitDetails
import SLED.GitExtraDep (CommitSHA (..), Repository (..))
import SLED.Hackage
import SLED.Options.Parse
import SLED.PackageName
import SLED.Run (runSuggestions)
import SLED.Stackage
import SLED.StackageResolver
import SLED.Suggestion.Format (defaultFormat)
import SLED.Test.Mocks
import SLED.Version
import Test.Hspec

data TestApp = TestApp
  { logger :: Logger
  , mocks :: Mocks
  }

instance HasLogger TestApp where
  loggerL = lens (.logger) $ \x y -> x {logger = y}

instance HasMocks TestApp where
  mocksL = lens (.mocks) $ \x y -> x {mocks = y}

newtype TestAppM a = TestAppM
  { unwrap :: ReaderT TestApp IO a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadReader TestApp
    )
  deriving
    ( MonadLogger
    , MonadLoggerIO
    )
    via (WithLogger TestApp IO)
  deriving
    ( MonadHackage
    , MonadStackage
    , MonadGit
    )
    via (ReaderMocks TestAppM)

runTestAppM :: TestAppM a -> IO a
runTestAppM f = do
  app <-
    TestApp
      <$> newTestLogger defaultLogSettings
      <*> pure emptyMocks

  runReaderT f.unwrap app

assertAutoFixed :: [Text] -> TestAppM ()
assertAutoFixed diff = do
  ctx <-
    execContextT input
      $ runSuggestions
      $ Options
        { path = "stack.yaml"
        , resolver = Nothing
        , contents = input
        , format = defaultFormat
        , excludes = []
        , checkResolver = True
        , checks = AllChecks
        , noExit = True
        , autoFix = True
        , filter = Nothing
        }

  liftIO $ ctx ^. contentsL `shouldBe` expected
 where
  (input, expected) = fromDiffLines diff

assertNoFixes :: [Text] -> TestAppM ()
assertNoFixes = assertAutoFixed . map toContext
 where
  toContext :: Text -> Text
  toContext t
    | T.null t = t
    | otherwise = "  " <> t

-- | Produce before/after from a simple diff
--
-- Given the lines:
--
-- @
-- [ " foo"
-- , "-bar"
-- , "+baz"
-- , " bat"
-- ]
-- @
--
-- it collects the context and removals on one side and the context and
-- additions on the other, producing the tuple:
--
-- @
-- ( "foo\nbar\nbat\n"
-- , "foo\nbaz\nbat\n"
-- )
-- @
fromDiffLines :: [Text] -> (ByteString, ByteString)
fromDiffLines = bimap encodeUtf8 encodeUtf8 . foldl' go ("", "")
 where
  go :: (Text, Text) -> Text -> (Text, Text)
  go (l, r) t = case T.uncons t of
    Nothing -> (l <> "\n", r <> "\n")
    Just (' ', rest) -> (l <> rest <> "\n", r <> rest <> "\n")
    Just ('-', rest) -> (l <> rest <> "\n", r)
    Just ('+', rest) -> (l, r <> rest <> "\n") -- after only
    Just (c, _) ->
      error
        $ "Invalid diff line: "
        <> show t
        <> "."
        <> "\nDiff lines must begin with space, -, or +, "
        <> "\nsaw "
        <> T.singleton c

instance IsString PackageName where
  fromString = PackageName . pack

instance IsString Version where
  fromString s = fromMaybe (error $ "Invalid version: " <> t) $ parseVersion t
   where
    t = pack s

instance IsString StackageResolver where
  fromString = stackageResolverFromText . pack

instance IsString Repository where
  fromString = Repository . pack

instance IsString CommitSHA where
  fromString = CommitSHA . pack

instance IsString t => IsString (Marked t) where
  fromString s =
    Marked
      { markedItem = fromString s
      , markedPath = "<input>"
      , markedJSONPath = Nothing
      , markedLocationStart = Location 0 0 0
      , markedLocationEnd = Location 0 0 0
      }
