{-# LANGUAGE FlexibleInstances #-}

module SLED.Test
  ( runTestChecks

    -- * Helpers
  , unsafeVersion
  , markAtZero

    -- * Fixtures
  , lts1818

    -- * Re-exports
  , module X
  ) where

import SLED.Prelude

import Blammo.Logging.Logger (newTestLogger)
import Data.List (elemIndex)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import SLED.Checks as X
import SLED.ExtraDep as X
import SLED.GitDetails
import SLED.GitExtraDep as X
import SLED.Hackage
import SLED.HackageExtraDep as X
import SLED.PackageName as X
import SLED.Run (runChecks)
import SLED.Stackage
import SLED.StackageResolver as X
import SLED.Suggestion as X
import SLED.Version
import Test.Hspec as X

newtype TestAppT app m a = TestAppT
  { unwrap :: ReaderT app (LoggingT m) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadLogger
    , MonadLoggerIO
    , MonadReader app
    )

instance Monad m => MonadHackage (TestAppT TestApp m) where
  getHackageVersions package = do
    m <- asks (.hackageVersionsByPackage)
    pure $ Map.lookup package m

instance Monad m => MonadStackage (TestAppT TestApp m) where
  getStackageVersions resolver package = do
    ms <- asks (.stackageVersionsByResolver)

    pure $ do
      m <- Map.lookup resolver ms
      Map.lookup package m

instance Monad m => MonadGit (TestAppT TestApp m) where
  gitClone _ _ = pure ()

  gitRevParse = \case
    "HEAD" -> withMockCommitSHAs $ (<> "\n") . encodeUtf8 . (.unwrap) . head
    x -> error $ pack $ "git rev-parse called with unexpected argument " <> x

  gitRevListCount spec = do
    headCommit <- T.strip . decodeUtf8 <$> gitRevParse "HEAD"
    let replaceHead x = if x == "HEAD" then headCommit else x

    withMockCommitSHAs $ \shas ->
      let (a, b) = T.breakOn ".." $ pack spec
      in  fromMaybe "" $ do
            let a' = replaceHead a
            b' <- replaceHead <$> T.stripPrefix ".." b
            nA <- CommitSHA a' `elemIndex` toList shas
            nB <- CommitSHA b' `elemIndex` toList shas
            pure $ (<> "\n") $ encodeUtf8 $ show @Text $ nA - nB

  gitForEachRef _ =
    withMockCommits
      $ encodeUtf8
      . mconcat
      . map (<> "\n")
      . mapMaybe (uncurry toRef)
      . toList
   where
    toRef :: CommitSHA -> Maybe Text -> Maybe Text
    toRef sha mTag = do
      t <- mTag
      pure $ "refs/tags/" <> t <> " " <> sha.unwrap

runTestAppT
  :: (MonadUnliftIO m, HasLogger app) => TestAppT app m a -> app -> m a
runTestAppT action app =
  runLoggerLoggingT app $ runReaderT action.unwrap app

data TestApp = TestApp
  { logger :: Logger
  , hackageVersionsByPackage :: Map PackageName HackageVersions
  , stackageVersionsByResolver
      :: Map StackageResolver (Map PackageName StackageVersions)
  , commits :: Maybe (NonEmpty (CommitSHA, Maybe Text))
  }

instance HasLogger TestApp where
  loggerL = lens (.logger) $ \x y -> x {logger = y}

withMockCommitSHAs
  :: (HasCallStack, MonadReader TestApp m) => (NonEmpty CommitSHA -> a) -> m a
withMockCommitSHAs f = withMockCommits $ f . fmap fst

withMockCommits
  :: (HasCallStack, MonadReader TestApp m)
  => (NonEmpty (CommitSHA, Maybe Text) -> a)
  -> m a
withMockCommits f = do
  mCommits <- asks (.commits)
  case mCommits of
    Nothing -> error "Git operation used without setting TestApp.commits"
    Just cs -> pure $ f cs

runTestChecks
  :: MonadUnliftIO m
  => Map PackageName HackageVersions
  -> Map StackageResolver (Map PackageName StackageVersions)
  -> Maybe (NonEmpty (CommitSHA, Maybe Text))
  -> Marked StackageResolver
  -> ChecksName
  -> ExtraDep
  -> m (Maybe SuggestionAction)
runTestChecks mockHackage mockStackage mockCommitSHAs resolver checksName extraDep = do
  testApp <-
    TestApp
      <$> newTestLogger defaultLogSettings
      <*> pure mockHackage
      <*> pure mockStackage
      <*> pure mockCommitSHAs

  let mextraDep =
        Marked
          { markedItem = extraDep
          , markedPath = "example.yaml"
          , markedLocationStart = Location 10 1 11
          , markedLocationEnd = Location 21 1 19
          }

  mSuggestion <- runTestAppT (runChecks resolver checksName mextraDep) testApp

  for mSuggestion $ \msuggestion -> do
    let suggestion = markedItem msuggestion

    liftIO $ do
      -- Assert the suggestion is for the right thing at the right mark
      void msuggestion `shouldBe` void mextraDep
      suggestion.target `shouldBe` extraDep

    pure $ suggestion.action

unsafeVersion :: HasCallStack => String -> Version
unsafeVersion s = fromMaybe err $ parseVersion s
 where
  err = error $ pack $ "Invalid version: " <> s

markAtZero :: a -> Marked a
markAtZero a =
  Marked
    { markedItem = a
    , markedPath = "<input>"
    , markedLocationStart = Location 0 0 0
    , markedLocationEnd = Location 0 0 0
    }

lts1818 :: Marked StackageResolver
lts1818 = markAtZero (StackageResolver "lts-18.18")
