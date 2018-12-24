import Test.Tasty
import Test.Tasty.HUnit

import Lex
import Translator
import Synt
import Utilities

main :: IO ()
main = do
    defaultMain $ testGroup "Tests" [lexTest, syntTest, translateTest,
        makeCFileNamesTest]
    -- TODO: add integration tests

lexTest :: TestTree
lexTest = testGroup "Lexical analysis"
    [makeTest emptyStruct tEmptyStruct pEmptyStruct,
     makeTest wrongId tWrongId pWrongId,
     makeTest underscoreId tUnderscoreId pUnderscoreId,
     makeTest apostropheId tApostropheId pApostropheId]
    where
        makeTest name tResult pSource = testCase name $ assertEqual name
            tResult $ alexScanTokens pSource

        emptyStruct = "empty structure"
        pEmptyStruct = "struct empty_struct {\n}\n"
        tEmptyStruct = Right [TStruct, TIdent "empty_struct", TLeftCrBrace,
                              TRightCrBrace, TEOF]

        wrongId = "wrong identifier"
        pWrongId = "struct 1empty_struct {\n}\n"
        tWrongId = Left "lexical error at line 1, column 8"

        underscoreId = "identifier with leading underscore"
        pUnderscoreId = "struct _empty_struct {\n}\n"
        tUnderscoreId = Left "lexical error at line 1, column 8"

        apostropheId = "identifier with apostrophe"
        pApostropheId = "struct rock'n'roll {\n}\n"
        tApostropheId = Left "lexical error at line 1, column 12"

syntTest :: TestTree
syntTest = testGroup "Syntax analysis"
    [testCase emptyStruct $ assertEqual emptyStruct
        pEmptyStruct $ synt tEmptyStruct]
    where
        emptyStruct = "empty structure"
        tEmptyStruct = [TStruct, TIdent "empty_struct", TLeftCrBrace,
                        TRightCrBrace, TEOF]
        pEmptyStruct = PTree $ PStruct "empty_struct"

translateTest :: TestTree
translateTest = testGroup "Translation"
    [testCase "translate" $ assertEqual dummyImplementation
        dummyCCode $ translate dummyPCode]
    where
        dummyImplementation = "Dummy implementation"
        dummyPCode = "blablabla"
        dummyCCode = Right ("Dummy C header code\n", "Dummy C source code\n")

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
