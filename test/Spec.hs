import Test.Tasty
import qualified IntcodeV1Spec
import qualified IntcodeV2Spec
import qualified D01P1Spec
import qualified D01P2Spec
import qualified D02P1Spec
import qualified D02P2Spec
import qualified D03P1Spec
import qualified D03P2Spec
import qualified D04P1Spec
import qualified D04P2Spec
import qualified D05P1Spec
import qualified D06P1Spec
import qualified D06P2Spec
import qualified D07P1Spec
import qualified D07P2Spec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [
        IntcodeV1Spec.tests
      , IntcodeV2Spec.tests
      , D01P1Spec.tests
      , D01P2Spec.tests
      , D02P1Spec.tests
      , D02P2Spec.tests
      , D03P1Spec.tests
      , D03P2Spec.tests
      , D04P1Spec.tests
      , D04P2Spec.tests
      , D05P1Spec.tests
      , D06P1Spec.tests
      , D06P2Spec.tests
      , D07P1Spec.tests
      , D07P2Spec.tests
    ]