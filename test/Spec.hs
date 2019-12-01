import Test.Tasty
import qualified D01P1Spec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [
        D01P1Spec.tests
    ]