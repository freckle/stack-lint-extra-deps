module Lsd.Run
  ( runLsd
  ) where

import RIO

import Lsd.Cache
import Lsd.Check
import Lsd.Checks
import Lsd.ExtraDep
import Lsd.Options
import Lsd.StackYaml
import Lsd.Suggestion
import RIO.Process
import System.FilePath.Glob

runLsd
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasLogFunc env
     , HasProcessContext env
     , HasCache env
     )
  => Options
  -> StackYaml
  -> (ExtraDep -> Suggestion -> m ())
  -> m Int
runLsd Options {..} StackYaml {..} report = do
  runChecks extraDeps checks report
 where
  extraDeps = filterExcludes oExcludes syExtraDeps
  resolver = fromMaybe syResolver oResolver
  checks = checksByName resolver oChecks

filterExcludes :: [Pattern] -> [ExtraDep] -> [ExtraDep]
filterExcludes excludes = filter $ \e -> not $ any (`matchExclude` e) excludes
