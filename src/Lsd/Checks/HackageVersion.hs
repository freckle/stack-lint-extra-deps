module Lsd.Checks.HackageVersion
  ( checkHackageVersion
  ) where

import RIO

import Control.Error.Util (hoistMaybe)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Lsd.Cache
import Lsd.Check
import Lsd.ExtraDep
import Lsd.HackageExtraDep
import Lsd.Stackage
import Lsd.StackageResolver
import Lsd.Suggestion

checkHackageVersion
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasCache env)
  => StackageResolver
  -> Check m
checkHackageVersion resolver = Check $ \extraDep -> do
  runMaybeT $ do
    Hackage HackageExtraDep {..} <- pure extraDep
    current <- hoistMaybe hedVersion
    StackageDetails {..} <- MaybeT $ getStackageDetails resolver hedPackage

    guard $ sdHackageVersion > current

    pure $ Suggestion
      { sAction = Replace
      , sDetails =
        "Newer version (" <> display sdHackageVersion <> ") is available"
      }
