module SLED.Check
  ( Check (..)

    -- * Re-exports useful for authoring 'Check's
  , module X
  ) where

import RIO

import SLED.ExternalDetails as X
import SLED.ExtraDep as X
import SLED.GitDetails as X
import SLED.GitExtraDep as X
import SLED.Hackage as X
import SLED.HackageExtraDep as X
import SLED.Stackage as X
import SLED.Suggestion as X

newtype Check = Check
  { runCheck :: ExternalDetails -> ExtraDep -> Maybe Suggestion
  }
