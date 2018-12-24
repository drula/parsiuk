-- | Translator module containing all translation functionality
module Translator (
    translate
    ) where

import Lex
import Synt
import Debug.Trace

-- TODO: use some special error type instead of String
-- | Translate Parsiuk code to C header code and C source code.
translate :: String -> Either String (String, String)
translate pSource = toTokens pSource >>= toPTree >>= toCTree >>= toCCode

-- | C AST. The type is not yet fully implemented.
data CTree = CTree CStruct -- ^ C Tree with C Structure description
    deriving (Eq, Show)
-- TODO: implement

-- | C Structure with its name. The type is not yet fully implemented.
data CStruct = CStruct String
    deriving (Eq, Show)

-- | Translate Parsiuk code to a list of tokens.
toTokens :: String -> Either String [Token]
toTokens = alexScanTokens

-- | Translate a list of Parsiuk tokens to Parsiuk AST.
toPTree :: [Token] -> Either String PTree
toPTree = synt

-- | Translate Parsiuk AST to C AST.
-- The function is not yet fully implemented.
toCTree :: PTree -> Either String CTree
toCTree (PTree pStruct) = trace (show result) $ Right $ result
    where result = CTree $ toCStruct pStruct
-- TODO: implement

-- | Translate Parsiuk structure to C structure
toCStruct :: PStruct -> CStruct
toCStruct (PStruct name) = CStruct name
-- TODO: implement
-- FIXME: add prefix to the name

-- | Translate C AST to C code (.h file and .c file)
-- The function is not yet implemented.
toCCode :: CTree -> Either String (String, String)
toCCode _cTree = Right ("Dummy C header code\n", "Dummy C source code\n")
-- TODO: implement
