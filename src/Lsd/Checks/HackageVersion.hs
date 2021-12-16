module Lsd.Checks.HackageVersion
  ( checkHackageVersion
  ) where

import RIO

import Control.Error.Util (hoistMaybe)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Lsd.Cache
import Lsd.Check
import Lsd.ExtraDep
import Lsd.Hackage
import Lsd.HackageExtraDep
import Lsd.PackageName
import Lsd.Stackage
import Lsd.StackageResolver
import Lsd.Suggestion
import Lsd.Version

checkHackageVersion
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasCache env)
  => StackageResolver
  -> Check m
checkHackageVersion resolver = Check $ \extraDep -> do
  runMaybeT $ do
    Hackage HackageExtraDep {..} <- pure extraDep
    current <- hoistMaybe hedVersion
    released <- getHackageVersion resolver hedPackage

    guard $ released > current

    pure $ Suggestion
      { sAction = Replace
      , sDetails = "Newer version (" <> display released <> ") is available"
      }

getHackageVersion
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasCache env)
  => StackageResolver
  -> PackageName
  -> MaybeT m Version
getHackageVersion resolver package = viaStackage <|> viaHackage
 where
  viaStackage =
    sdHackageVersion <$> MaybeT (getStackageDetails resolver package)

  viaHackage = hdVersion <$> MaybeT (getHackageDetails package)
