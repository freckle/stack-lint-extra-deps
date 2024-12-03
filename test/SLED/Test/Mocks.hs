{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

module SLED.Test.Mocks
  ( Mocks
  , emptyMocks

    -- * @DerivingVia@
  , HasMocks (..)
  , ReaderMocks (..)

    -- * Helpers
  , withHackage
  , withHackageVersions
  , withStackage
  , withStackageVersions
  , withStackageResolvers
  , withGitClone
  ) where

import SLED.Prelude

import Control.Lens (at, non, (<>~), (?~))
import Data.List.Extra (splitOn)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import SLED.GitDetails
import SLED.GitExtraDep
import SLED.Hackage
import SLED.PackageName
import SLED.Stackage
import SLED.StackageResolver
import SLED.Version

data Mocks = Mocks
  { hackage :: Map PackageName HackageVersions
  , stackage :: StackageMocks
  , git :: GitMocks
  }

emptyMocks :: Mocks
emptyMocks =
  Mocks
    { hackage = Map.empty
    , stackage =
        StackageMocks
          { versions = Map.empty
          , resolvers = []
          }
    , git =
        GitMocks
          { inClone = Nothing
          , commits = Map.empty
          }
    }

hackageL :: Lens' Mocks (Map PackageName HackageVersions)
hackageL = lens (.hackage) $ \x y -> x {hackage = y}

stackageL :: Lens' Mocks StackageMocks
stackageL = lens (.stackage) $ \x y -> x {stackage = y}

gitL :: Lens' Mocks GitMocks
gitL = lens (.git) $ \x y -> x {git = y}

data StackageMocks = StackageMocks
  { versions :: Map StackageResolver (Map PackageName StackageVersions)
  , resolvers :: [StackageResolver]
  }

versionsL
  :: Lens' StackageMocks (Map StackageResolver (Map PackageName StackageVersions))
versionsL = lens (.versions) $ \x y -> x {versions = y}

resolversL :: Lens' StackageMocks [StackageResolver]
resolversL = lens (.resolvers) $ \x y -> x {resolvers = y}

data GitMocks = GitMocks
  { inClone :: Maybe String
  -- ^ Tracks what clone we're in when things are called
  , commits :: Map String [(CommitSHA, Maybe Text)]
  -- ^ For responding with the commits for the current clone we're in
  }

inCloneL :: Lens' GitMocks (Maybe String)
inCloneL = lens (.inClone) $ \x y -> x {inClone = y}

commitsL :: Lens' GitMocks (Map String [(CommitSHA, Maybe Text)])
commitsL = lens (.commits) $ \x y -> x {commits = y}

viewCommits
  :: (MonadReader env m, HasMocks env, HasCallStack) => m [(CommitSHA, Maybe Text)]
viewCommits = do
  GitMocks {inClone, commits} <- view $ mocksL . gitL

  let
    msg :: Text
    msg = unlines $ case inClone of
      Nothing ->
        [ "Git operation called outside of gitClone."
        , "This is either a bug in the implementation's git operations, "
        , "or a bug in the test implementation of gitClone."
        ]
      Just c ->
        [ "Git operation called while in a clone of " <> pack c <> "."
        , "Commits are currently mocked for: "
            <> T.intercalate ", " (map pack $ Map.keys commits)
            <> "."
        , "Add or updated withGitClone to be the correct URL."
        ]

  pure $ fromMaybe (error msg) $ flip Map.lookup commits =<< inClone

class HasMocks env where
  mocksL :: Lens' env Mocks

instance HasMocks Mocks where
  mocksL = id

newtype ReaderMocks m a = ReaderMocks
  { unwrap :: m a
  }
  deriving newtype (Functor, Applicative, Monad, MonadReader env)

instance (MonadReader env m, HasMocks env) => MonadHackage (ReaderMocks m) where
  getHackageVersions package = view $ mocksL . hackageL . at package

instance (MonadReader env m, HasMocks env) => MonadStackage (ReaderMocks m) where
  getStackageVersions resolver package =
    view
      $ mocksL
      . stackageL
      . versionsL
      . at resolver
      . non Map.empty
      . at package

  getLatestInSeries resolver = do
    resolvers <- view $ mocksL . stackageL . resolversL
    pure
      $ fromMaybe resolver
      $ find (stackageResolverSameSeries resolver) resolvers

instance (MonadReader env m, HasMocks env) => MonadGit (ReaderMocks m) where
  gitClone url _path = local $ mocksL . gitL . inCloneL ?~ url

  -- Given HEAD, return the first sha in commits, as line-terminated ByteString
  gitRevParse = \case
    "HEAD" -> do
      commits <- nonEmpty <$> viewCommits
      pure $ maybe "\n" (commitToLBS . fst . head) commits
    x -> error $ "gitRevParse is only mocked for HEAD, saw: " <> show x
   where
    commitToLBS c = fromStrict $ encodeUtf8 c.unwrap <> "\n"

  -- Given a..b, return count between a and b in commits, as line-terminated
  -- ByteString. If a or b is HEAD use the head commit. Most failures come out
  -- as "0\n".
  gitRevListCount spec = do
    commits <- viewCommits

    let
      indexed :: [(Int, CommitSHA)]
      indexed = zip [0 ..] $ map fst commits

      specPartToCommitSHA :: String -> CommitSHA
      specPartToCommitSHA x = case (x, nonEmpty commits) of
        ("HEAD", Just ((sha, _) :| _)) -> sha
        _ -> CommitSHA $ pack x

    pure $ (<> "\n") $ fromMaybe "0" $ do
      [a, b] <- Just $ map specPartToCommitSHA $ splitOn ".." spec
      ia <- fst <$> find ((== a) . snd) indexed
      ib <- fst <$> find ((== b) . snd) indexed
      guard $ ia >= ib
      pure $ encodeUtf8 $ show @Text $ ia - ib

  -- Return each commit with a tag as "refs/tags/{tag} {sha}"
  gitForEachRef _ =
    fromStrict . encodeUtf8 . T.unlines . mapMaybe (uncurry go) <$> viewCommits
   where
    go :: CommitSHA -> Maybe Text -> Maybe Text
    go sha mTag = do
      tag <- mTag
      pure $ "refs/tags/" <> tag <> " " <> sha.unwrap

withHackage
  :: (MonadReader env m, HasMocks env)
  => PackageName
  -> Version
  -> m a
  -> m a
withHackage package version =
  withHackageVersions package
    $ HackageVersions
      { normal = [version]
      , unpreferred = []
      , deprecated = []
      }

withHackageVersions
  :: (MonadReader env m, HasMocks env)
  => PackageName
  -> HackageVersions
  -> m a
  -> m a
withHackageVersions package versions =
  local $ mocksL . hackageL . at package ?~ versions

withStackage
  :: (MonadReader env m, HasMocks env)
  => StackageResolver
  -> PackageName
  -> Version
  -> m a
  -> m a
withStackage resolver package version =
  withStackageVersions resolver package
    $ StackageVersions
      { onPage = version
      , onHackage = error "onHackage not defined"
      }

withStackageVersions
  :: (MonadReader env m, HasMocks env)
  => StackageResolver
  -> PackageName
  -> StackageVersions
  -> m a
  -> m a
withStackageVersions resolver package versions =
  local
    $ mocksL
    . stackageL
    . versionsL
    . at resolver
    . non Map.empty
    . at package
    ?~ versions

withStackageResolvers
  :: (MonadReader env m, HasMocks env)
  => [StackageResolver]
  -> m a
  -> m a
withStackageResolvers resolvers =
  local $ mocksL . stackageL . resolversL <>~ resolvers

withGitClone
  :: (MonadReader env m, HasMocks env)
  => String
  -- ^ URL
  -> [(CommitSHA, Maybe Text)]
  -> m a
  -> m a
withGitClone url commits =
  local $ mocksL . gitL . commitsL . at url ?~ commits
