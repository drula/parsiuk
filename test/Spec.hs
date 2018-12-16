import Test.Tasty
import Test.Tasty.HUnit

import Translator
import Utilities

main :: IO ()
main = do
    defaultMain $ testGroup "Tests" [translateTest, makeCFileNamesTest]

translateTest :: TestTree
translateTest = testCase "Translation" $
    assertEqual "Checking if 'translate' is implemented"
        expectedResult $ translate "blablabla"
    where expectedResult = Left "The function 'translate' is not yet implemented"

makeCFileNamesTest :: TestTree
makeCFileNamesTest = testGroup "Making C file names from Parsiuk file names"
    [makeTest "test.prl" "test.h" "test.c",
     makeTest "prl.prl" "prl.h" "prl.c",
     makeTest "prl.abc" "prl.h" "prl.c",
     makeTest "abc" "abc.h" "abc.c",
     makeTest "prl" "prl.h" "prl.c",
     makeTest "test.abc" "test.h" "test.c",
     makeTest "test.h" "test_h.h" "test_h.c",
     makeTest "test.c" "test_c.h" "test_c.c",
     makeTest ".prl" ".h" ".c",
     makeTest "abc." "abc.h" "abc.c",
     makeTest "." ".h" ".c",
     makeTest "" ".h" ".c"]
    where
        makeTest pSource cHeader cSource =
            let testName = pSource ++ " => " ++ cHeader ++ ", " ++ cSource in
            testCase testName $
                assertEqual pSource (cHeader, cSource) $ makeCFileNames pSource