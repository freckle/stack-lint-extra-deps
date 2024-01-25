module SLED.Check
  ( Check (..)
  , runCheck

    -- * Re-exports useful for authoring 'Check's
  , module X
  ) where

import SLED.Prelude

import Data.Yaml.Marked.Replace as X
import SLED.ExternalDetails as X
import SLED.ExtraDep as X
import SLED.GitDetails as X
import SLED.GitExtraDep as X
import SLED.Hackage as X
import SLED.HackageExtraDep as X
import SLED.Stackage as X
import SLED.Suggestion as X

newtype Check = Check
  { run :: ExternalDetails -> ExtraDep -> Maybe SuggestionAction
  }

runCheck
  :: ExternalDetails -> Marked ExtraDep -> Check -> Maybe (Marked Suggestion)
runCheck ed mextraDep check = do
  let extraDep = markedItem mextraDep

  action <- check.run ed extraDep

  let suggestion =
        Suggestion
          { target = extraDep
          , action = action
          }

  pure $ suggestion <$ mextraDep
