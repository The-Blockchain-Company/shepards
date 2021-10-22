-- | The top-level module of shepards, which re-exports the main functionality.
module Test.Shepard
  ( module Test.Shepard.Core
  , module Test.Shepard.Explainer
  , module Test.Shepard.Persist
  ) where

import Test.Shepard.Core
import Test.Shepard.Explainer
import Test.Shepard.Instances ()
import Test.Shepard.Persist
