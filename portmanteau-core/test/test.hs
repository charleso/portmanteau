import           Control.Monad (unless)
import           System.Exit (exitFailure)
import           System.IO (BufferMode (..), stdout, stderr, hSetBuffering)

import qualified Test.Portmanteau.Core.Codec


main :: IO ()
main =
  testAll [
      Test.Portmanteau.Core.Codec.tests
    ]


testAll :: [IO Bool] -> IO ()
testAll tests = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  sequence tests >>= \rs -> unless (and rs) exitFailure
