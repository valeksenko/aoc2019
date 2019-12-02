import Test.Tasty
import qualified D01P1Spec
import qualified D01P2Spec
import qualified D02Spec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [
        D01P1Spec.tests
      , D01P2Spec.tests
      , D02Spec.tests
    ]