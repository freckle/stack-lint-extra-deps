module Check
  ( Check (..)

    -- * Re-exports useful for authoring 'Check's
  , module X
  ) where

import RIO

import ExternalDetails as X
import ExtraDep as X
import GitDetails as X
import GitExtraDep as X
import Hackage as X
import HackageExtraDep as X
import Stackage as X
import Suggestion as X

newtype Check = Check
  { runCheck :: ExternalDetails -> ExtraDep -> Maybe Suggestion
  }
