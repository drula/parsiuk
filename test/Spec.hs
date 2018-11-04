import Test.Tasty
import Test.Tasty.HUnit

import Translator (translate)

main :: IO ()
main = do
    defaultMain $ testGroup "Translator tests" [translateTest]

translateTest :: TestTree
translateTest = testCase "Testing translate" $
    assertEqual "Checking if 'translate' is implemented" expectedResult (translate "blablabla")
    where expectedResult = Left "The function is not yet implemented"
