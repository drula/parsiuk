-- | Module for generation C AST from Parsiuk AST
module CTreeGen (
    CTree(..),
    CStruct(..),
    toCTree
    ) where

import Synt

-- | C AST. The type is not yet fully implemented.
data CTree = CTree CStruct -- ^ C Tree with C Structure description
    deriving (Eq, Show)
-- TODO: implement

-- | C Structure with its name. The type is not yet fully implemented.
data CStruct = CStruct String
    deriving (Eq, Show)

-- | Translate Parsiuk AST to C AST.
-- The function is not yet fully implemented.
toCTree :: PTree -> Either String CTree
toCTree (PTree pStruct) = Right $  CTree $ toCStruct pStruct
-- TODO: implement

-- | Translate Parsiuk structure to C structure.
-- The function is not yet fully implemented.
toCStruct :: PStruct -> CStruct
toCStruct (PStruct name) = CStruct name
-- TODO: implement
-- FIXME: add prefix to the name
