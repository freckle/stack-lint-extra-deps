{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module SLED.Context
  ( Context
  , execContextT
  , seenL
  , contentsL
  , whenUnseen
  , untilNoneSeen
  , sequenceFirstSeen
  , rewriteContents
  ) where

import SLED.Prelude hiding ((.=))

import Control.Lens (views, (.=))
import Data.DList (DList)

data Context = Context
  { seen :: DList (Marked Bool)
  -- ^ Tracking 'Suggestion's made so far, so we don't re-make them
  --
  -- The 'Bool' indicates if the suggestion was auto-fixed. We can't hold the
  -- target itself because its type will change throughout.
  , contents :: ByteString
  -- ^ @stack.yaml@ contents, updated with any auto-fixes
  }

execContextT :: Monad m => ByteString -> StateT Context m a -> m Context
execContextT contents f = execStateT f $ Context {seen = mempty, contents}

seenL :: Lens' Context (DList (Marked Bool))
seenL = lens (.seen) $ \x y -> x {seen = y}

contentsL :: Lens' Context ByteString
contentsL = lens (.contents) $ \x y -> x {contents = y}

-- | Run an action unless the item is already present in 'seen'
whenUnseen
  :: MonadState Context m => (Marked a -> m ()) -> Marked a -> m ()
whenUnseen act m = do
  unseen <- gets $ views seenL $ \ms -> void m `notElem` fmap void ms
  when unseen $ act m

-- | Repeat an action until it no longer adds 'seen' values
untilNoneSeen :: MonadState Context m => m () -> m ()
untilNoneSeen act = do
  new <- runAndCheckSeen act
  when new $ untilNoneSeen act

-- | Run a list of actions stopping after any new value is added to 'seen'
sequenceFirstSeen :: MonadState Context m => [m ()] -> m ()
sequenceFirstSeen = \case
  [] -> pure ()
  f : fs -> do
    new <- runAndCheckSeen f
    unless new $ sequenceFirstSeen fs

runAndCheckSeen :: MonadState Context m => m () -> m Bool
runAndCheckSeen act = do
  before <- gets $ views seenL length
  act
  gets $ views seenL $ (> before) . length

rewriteContents :: MonadState Context m => (ByteString -> m ByteString) -> m ()
rewriteContents = overM contentsL

overM :: MonadState s m => Lens' s b -> (b -> m b) -> m ()
overM l f = do
  x <- gets $ view l
  void $ (l .=) =<< f x
