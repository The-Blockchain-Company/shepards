import Control.Monad (unless)
import Hedgehog
import System.Exit (exitFailure)
import System.IO (hSetEncoding, stderr, stdout, utf8)

import Test.Shepard.Properties

-- | Main testing action
main :: IO ()
main = runTests $ checkSequential <$>
  [ Test.Shepard.Properties.tests
  ]

-- Lifted from `bcc-prelude`:
-- https://github.com/The-Blockchain-Company/bcc-prelude/blob/d2a4f06827bfa11c021ce719285e8d0bb6ac8e44/test/Test/Bcc/Prelude/Tripping.hs#L141
runTests :: [IO Bool] -> IO ()
runTests tests' = do
  -- ensure UTF-8. As that's what hedgehog needs.
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  result <- and <$> sequence tests'
  unless result exitFailure
