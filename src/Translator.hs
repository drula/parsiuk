-- | Translator module containing all translation functionality
module Translator (
    translate
    ) where

import Lex
import Synt

-- TODO: use some special error type instead of String
-- | Translate Parsiuk code to C header code and C source code.
translate :: String -> Either String (String, String)
translate pSource = toTokens pSource >>= toPTree >>= toCTree >>= toCCode

-- | C AST. The type is not yet implemented.
data CTree = CTree -- ^ Dummy C Tree constructor
    deriving (Eq, Show)
-- TODO: implement

-- | Translate Parsiuk code to a list of tokens.
toTokens :: String -> Either String [Token]
toTokens = alexScanTokens

-- | Translate a list of Parsiuk tokens to Parsiuk AST.
-- The function is not yet implemented.
toPTree :: [Token] -> Either String PTree
toPTree tokens = Right $ synt tokens
-- TODO: implement

-- | Translate Parsiuk AST to C AST.
-- The function is not yet implemented.
toCTree :: PTree -> Either String CTree
toCTree _pTree = Right CTree
-- TODO: implement

-- | Translate C AST to C code (.h file and .c file)
-- The function is not yet implemented.
toCCode :: CTree -> Either String (String, String)
toCCode _cTree = Right ("Dummy C header code\n", "Dummy C source code\n")
-- TODO: implement
