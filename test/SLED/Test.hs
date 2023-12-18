{-# LANGUAGE FlexibleInstances #-}

module SLED.Test
  ( runTestChecks

    -- * Helpers
  , unsafeVersion
  , toHackageExtraDepUnsafe

    -- * Fixtures
  , lts1818
  , freckleApp1011
  , yesodFlowRoutesGitHub

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
import SLED.GitExtraDep
import SLED.Hackage
import SLED.HackageExtraDep
import SLED.PackageName as X
import SLED.Run (runChecks)
import SLED.Stackage
import SLED.StackageResolver as X
import SLED.Suggestion as X
import SLED.Version
import Test.Hspec as X

newtype TestAppT app m a = TestAppT
  { unTestAppT :: ReaderT app (LoggingT m) a
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
    m <- asks taHackageVersionsByPackage
    pure $ Map.lookup package m

instance Monad m => MonadStackage (TestAppT TestApp m) where
  getStackageVersions resolver package = do
    ms <- asks taStackageVersionsByResolver

    pure $ do
      m <- Map.lookup resolver ms
      Map.lookup package m

instance Monad m => MonadGit (TestAppT TestApp m) where
  gitClone _ _ = pure ()

  gitRevParse = \case
    "HEAD" -> withMockCommitSHAs $ (<> "\n") . encodeUtf8 . unCommitSHA . head
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
      pure $ "refs/tags/" <> t <> " " <> unCommitSHA sha

runTestAppT
  :: (MonadUnliftIO m, HasLogger app) => TestAppT app m a -> app -> m a
runTestAppT action app =
  runLoggerLoggingT app $ runReaderT (unTestAppT action) app

data TestApp = TestApp
  { taLogger :: Logger
  , taHackageVersionsByPackage :: Map PackageName HackageVersions
  , taStackageVersionsByResolver
      :: Map StackageResolver (Map PackageName StackageVersions)
  , taCommits :: Maybe (NonEmpty (CommitSHA, Maybe Text))
  }

instance HasLogger TestApp where
  loggerL = lens taLogger $ \x y -> x {taLogger = y}

withMockCommitSHAs
  :: (HasCallStack, MonadReader TestApp m) => (NonEmpty CommitSHA -> a) -> m a
withMockCommitSHAs f = withMockCommits $ f . fmap fst

withMockCommits
  :: (HasCallStack, MonadReader TestApp m)
  => (NonEmpty (CommitSHA, Maybe Text) -> a)
  -> m a
withMockCommits f = do
  mCommits <- asks taCommits
  case mCommits of
    Nothing -> error "Git operation used without setting taCommitSHAs"
    Just cs -> pure $ f cs

runTestChecks
  :: MonadUnliftIO m
  => Map PackageName HackageVersions
  -> Map StackageResolver (Map PackageName StackageVersions)
  -> Maybe (NonEmpty (CommitSHA, Maybe Text))
  -> Marked StackageResolver
  -> ChecksName
  -> Marked ExtraDep
  -> m [Suggestion]
runTestChecks mockHackage mockStackage mockCommitSHAs resolver checksName extraDep = do
  testApp <-
    TestApp
      <$> newTestLogger defaultLogSettings
      <*> pure mockHackage
      <*> pure mockStackage
      <*> pure mockCommitSHAs

  runTestAppT (runChecks resolver checksName extraDep) testApp

unsafeVersion :: HasCallStack => String -> Version
unsafeVersion s = fromMaybe err $ parseVersion s
 where
  err = error $ pack $ "Invalid version: " <> s

toHackageExtraDepUnsafe :: HasCallStack => ExtraDep -> HackageExtraDep
toHackageExtraDepUnsafe = \case
  Hackage x -> x
  x -> error $ "Expected HackageExtraDep, got: " <> show x

lts1818 :: Marked StackageResolver
lts1818 = markAtZero (StackageResolver "lts-18.18") "<input>"

freckleApp1011 :: HackageExtraDep
freckleApp1011 =
  HackageExtraDep
    { hedPackage = PackageName "freckle-app"
    , hedVersion = Just $ unsafeVersion "1.0.1.1"
    , hedChecksum = Nothing
    }

yesodFlowRoutesGitHub :: GitExtraDep
yesodFlowRoutesGitHub =
  GitExtraDep
    { gedRepository = Repository "https://github.com/freckle/yesod-routes-flow"
    , gedCommit =
        markAtZero (CommitSHA "2a9cd873880956dd9a0999b593022d3c746324e8") "<input>"
    }
