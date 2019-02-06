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

{-
    Prefixes:

pSrc  - Parsiuk source code
pTok  - Parsiuk tokens
pTree - Parsiuk tree

-}

lexTest :: TestTree
lexTest = testGroup "Lexical analysis"
    [makeTest emptyStruct (Right pTokEmptyStruct) pSrcEmptyStruct,
     makeTest "wrong identifier" pTokWrongId pSrcWrongId,
     makeTest "identifier with leading underscore" pTokUnderscoreId pSrcUnderscoreId,
     makeTest "identifier with apostrophe" pTokApostropheId pSrcApostropheId]
    where
        makeTest name tResult pSource = testCase name $ assertEqual name
            tResult $ toTokens pSource

        makeLexicalError line column = Left $ "lexical error at line " ++
                                       show line ++ ", column " ++ show column

        pSrcWrongId = makeEmptyStruct "1empty_struct"
        pTokWrongId = makeLexicalError 1 8

        pSrcUnderscoreId = makeEmptyStruct "_empty_struct"
        pTokUnderscoreId = makeLexicalError 1 8

        pSrcApostropheId = makeEmptyStruct "rock'n'roll"
        pTokApostropheId = makeLexicalError 1 12

syntTest :: TestTree
syntTest = testGroup "Syntax analysis"
    [makeTest emptyStruct pTreeEmptyStruct pTokEmptyStruct,
     makeTest "no initial 'struct' keyword" pTreeNoInitStruct pTokNoInitStruct,
     makeTest "no structure name" pTreeNoStructId pTokNoStructId,
     makeTest "no opening {" pTreeNoOpenCrBrace pTokNoOpenCrBrace,
     makeTest "no closing }" pTreeNoClosingCrBrace pTokNoClosingCrBrace]
    where
        makeTest name expectedResult testData = testCase name $ assertEqual name
            expectedResult $ toPTree testData

        makeSyntaxError tokList = Left $ "Syntax error: " ++ show tokList

        pTreeEmptyStruct = Right $ PTree $ PStruct pScrEmptyStructName

        pTokNoInitStruct = [TIdent pScrEmptyStructName, TLeftCrBrace, TRightCrBrace, TEOF]
        pTreeNoInitStruct = makeSyntaxError pTokNoInitStruct

        pTokNoStructId = [TStruct, TLeftCrBrace, TRightCrBrace, TEOF]
        pTreeNoStructId = makeSyntaxError $ tail pTokNoStructId

        pTokNoOpenCrBrace = [TStruct, TIdent pScrEmptyStructName, TRightCrBrace, TEOF]
        pTreeNoOpenCrBrace = makeSyntaxError [TRightCrBrace, TEOF]

        pTokNoClosingCrBrace = [TStruct, TIdent pScrEmptyStructName, TLeftCrBrace, TEOF]
        pTreeNoClosingCrBrace = makeSyntaxError [TEOF]

translateTest :: TestTree
translateTest = testGroup "Translation"
    [testCase "translate" $ assertEqual dummyImplementation
        dummyCCode $ translate pSrcEmptyStruct "test"]
        -- TODO: add a test with zero prefix
    where
        dummyImplementation = "Dummy implementation"
        dummyCCode = Right (emptyCHeader, "Dummy C source code\n")
        emptyCHeader = "typedef struct test_empty_struct {\n} test_empty_struct_t;\n"

emptyStruct = "empty structure"
pScrEmptyStructName = "empty_struct"
pSrcEmptyStruct = makeEmptyStruct pScrEmptyStructName
pTokEmptyStruct = [TStruct, TIdent pScrEmptyStructName, TLeftCrBrace,
                   TRightCrBrace, TEOF]

makeEmptyStruct :: String -> String
makeEmptyStruct name = "struct " ++ name ++ " {\n}\n"

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
