module Lsd.Checks.RedundantHackage
  ( checkRedundantHackage
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

checkRedundantHackage
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasCache env)
  => StackageResolver
  -> Check m
checkRedundantHackage resolver = Check $ \extraDep -> do
  runMaybeT $ do
    Hackage HackageExtraDep {..} <- pure extraDep
    current <- hoistMaybe hedVersion
    StackageDetails {..} <- MaybeT $ getStackageDetails resolver hedPackage

    guard $ sdStackageVersion >= current

    pure $ Suggestion
      { sAction = Remove
      , sDetails =
        "Newer version ("
        <> display sdStackageVersion
        <> ") is already in your resolver"
      }
