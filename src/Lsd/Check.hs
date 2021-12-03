module Lsd.Check
    ( Check(..)
    , runChecks
    ) where

import RIO

import Lsd.ExtraDep
import Lsd.Suggestion

newtype Check m = Check
    { runCheck :: ExtraDep -> m (Maybe Suggestion)
    }

runChecks
    :: Monad m
    => [ExtraDep]
    -> [Check m]
    -> (ExtraDep -> Suggestion -> m ())
    -> m Int
runChecks extraDeps checks onSuggestion = do
    fmap (length . catMaybes) $ for pairs $ \(check, extraDep) -> do
        mSuggestion <- runCheck check extraDep
        traverse (onSuggestion extraDep) mSuggestion
    where pairs = (,) <$> checks <*> extraDeps
