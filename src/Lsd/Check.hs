module Lsd.Check
  ( Check(..)

  -- * Re-exports useful for authoring 'Check's
  , module X
  ) where

import RIO

import Lsd.ExternalDetails as X
import Lsd.ExtraDep as X
import Lsd.GitDetails as X
import Lsd.GitExtraDep as X
import Lsd.Hackage as X
import Lsd.HackageExtraDep as X
import Lsd.Stackage as X
import Lsd.Suggestion as X

newtype Check = Check
  { runCheck :: ExternalDetails -> ExtraDep -> Maybe Suggestion
  }
