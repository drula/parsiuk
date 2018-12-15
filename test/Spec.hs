import Test.Tasty
import Test.Tasty.HUnit

import Translator
import Utilities

main :: IO ()
main = do
    defaultMain $ testGroup "Translator tests" [translateTest, makeCFileNamesTest]

translateTest :: TestTree
translateTest = testCase "Testing translate" $
    assertEqual "Checking if 'translate' is implemented" (translate "blablabla") expectedResult
    where expectedResult = Left "The function 'translate' is not yet implemented"

makeCFileNamesTest :: TestTree
makeCFileNamesTest = testCase "Making C file names from Parsiuk file names" $
    assertEqual "test.prl" (makeCFileNames "test.prl") ("test.h", "test.c")
    -- TODO: add more tests
