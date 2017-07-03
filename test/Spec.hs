import Test.HUnit ( runTestTT )

import qualified TestAuthentication as TestAuthentication

main :: IO ()
main = do
    runTestTT TestAuthentication.tests
    pure ()
