import           Control.Monad (unless)
import           System.Exit (exitFailure)
import           System.IO (BufferMode (..), stdout, stderr, hSetBuffering)

import qualified Test.Portmanteau.Aeson


main :: IO ()
main =
  testAll [
      Test.Portmanteau.Aeson.tests
    ]


testAll :: [IO Bool] -> IO ()
testAll tests = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  sequence tests >>= \rs -> unless (and rs) exitFailure
