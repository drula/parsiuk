-- | Translator module containing all translation functionality
module Translator (
    PTree(..),
    CTree(..),
    translate,
    toPTree,
    toCTree,
    toCCode,
    ) where

-- TODO: use some special error type instead of String
-- | Translate Parsiuk code to C header code and C source code.
translate :: String -> Either String (String, String)
translate pSource = toPTree pSource >>= toCTree >>= toCCode

-- | Parsiuk AST. The type is not yet implemented.
data PTree = PTree -- ^ Dummy Parsiuk Tree constructor
    deriving (Eq, Show)
-- TODO: implement

-- | C AST. The type is not yet implemented.
data CTree = CTree -- ^ Dummy C Tree constructor
    deriving (Eq, Show)
-- TODO: implement

-- | Translate Parsiuk code to Parsiuk AST.
-- The function is not yet implemented.
toPTree :: String -> Either String PTree
toPTree _ = Right PTree
-- TODO: implement

-- | Translate Parsiuk AST to C AST.
-- The function is not yet implemented.
toCTree :: PTree -> Either String CTree
toCTree _ = Right CTree
-- TODO: implement

-- | Translate C AST to C code (.h file and .c file)
-- The function is not yet implemented.
toCCode :: CTree -> Either String (String, String)
toCCode _ = Right ("Dummy C header code\n", "Dummy C source code\n")
-- TODO: implement
