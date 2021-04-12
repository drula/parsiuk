-- | Module for generation C AST from Parsiuk AST
module CTreeGen (
    CTree(..),
    CStruct(..),
    CFunction(..),
    CFnHeader(..),
    CVarDecl(..),
    CInstruction(..),
    CType(..),
    toCTree
    ) where

import Synt

-- | C AST. The type is not yet fully implemented.
data CTree = CTree CStruct [CFunction] -- ^ C Tree with C structure and functions description
    deriving (Eq, Show)
-- TODO: implement

-- | C structure with its name. The type is not yet fully implemented.
data CStruct = CStruct String
    deriving (Eq, Show)

-- | C function with its header and instruction list.
data CFunction = CFunction CFnHeader [CInstruction]
    deriving (Eq, Show)

-- | C function header: return value type, function name, parameter list.
data CFnHeader = CFnHeader CType String [CVarDecl]
    deriving (Eq, Show)

-- | C variable declaration: type and variable name.
data CVarDecl = CVarDecl CType String
    deriving (Eq, Show)

-- | C instruction. The type is not yet fully implemented.
data CInstruction = CInstruction
    deriving (Eq, Show)

-- | C value type.
data CType = CResultT -- ^ `prs_result_t` (error code type)
           | CUint8T -- ^ `uint8_t`
           | CSizeT -- ^ `size_t`
           | CVoidT -- ^ `void`
           | CPtrT CType -- ^ `*` type
           | CConstT CType -- ^ `const` type
           | CUserT String -- ^ user type (resulting structure)
    deriving (Eq, Show)

-- | Translate Parsiuk AST to C AST.
-- The function is not yet fully implemented.
toCTree :: PTree -> Either String CTree
toCTree (PTree pStruct) = Right $ CTree (toCStruct pStruct)
                          [toParserFn pStruct, toFreeFn pStruct]
-- TODO: implement

-- | Translate Parsiuk structure to parser function.
-- The function is not yet fully implemented.
toParserFn :: PStruct -> CFunction
toParserFn (PStruct name) = CFunction parseHdr []
    where parseHdr = CFnHeader CResultT (name ++ "_parse")
                     [CVarDecl (CPtrT (CConstT CUint8T)) "data",
                      CVarDecl CSizeT "size",
                      CVarDecl (CPtrT (CPtrT (CUserT $ name ++ "_t"))) "out"] -- FIXME: other variable name
-- TODO: implement

-- | Translate Parsiuk structure to the function freeing C structure.
-- The function is not yet fully implemented.
toFreeFn :: PStruct -> CFunction
toFreeFn (PStruct name) = CFunction freeHdr []
    where freeHdr = CFnHeader CVoidT (name ++ "_free")
                    [CVarDecl (CPtrT (CUserT $ name ++ "_t")) "p"] -- FIXME: other variable name
-- TODO: implement

-- | Translate Parsiuk structure to C structure.
-- The function is not yet fully implemented.
toCStruct :: PStruct -> CStruct
toCStruct (PStruct name) = CStruct name
-- TODO: implement
-- FIXME: add prefix to the name
