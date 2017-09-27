import           System.Environment
import           Test.Tasty

import qualified Data.Format.Test

main :: IO ()
main = do
  defaultMain $
    testGroup "Test Suite"
    [ Data.Format.Test.tests
    ]
