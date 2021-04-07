-- | Module for generating C code from C AST
module Texter (
    toCCode
    ) where

import Data.List (intercalate)
import CTreeGen

-- | The class of types that can be represented as a text (a list of strings)
-- (especially for CTree parts).
class Stringified a where
    -- | Represent a value as a text
    stringify :: a -> [String]

-- | Text representation for CStruct.
-- The function is not yet fully implemented.
instance Stringified CStruct where
    stringify (CStruct name) = ["typedef struct " ++ name ++ " {",
                                "} " ++ name ++ "_t;"]
    -- TODO: implement

instance Stringified CFunction where
    stringify (CFunction header instructions) = (show header ++ " {") : ["}"]
    -- TODO: implement

instance Show CFnHeader where
    show (CFnHeader typ name varDecls) = show typ ++ name ++ "(" ++ varList ++ ")"
        where varList = case varDecls of
                        [] -> "void"
                        _ -> intercalate ", " $ map show varDecls

instance Show CVarDecl where
    show (CVarDecl typ name) = show typ ++ name

instance Show CType where
    show CResultT = "prs_result_t "
    show CUint8T = "uint8_t "
    show CSizeT = "size_t "
    show CVoidT = "void "
    show (CPtrT typ) = show typ ++ "*"
    show (CConstT typ) = show typ ++ "const "
    show (CUserT name) = name ++ " "

-- | The class of types that need prefixation: adding prefixes
-- to user types and function names.
class Prefixable a where
    prefixate :: String -> a -> a

-- | Translate C AST to C code (.h file and .c file) with prefix addition.
toCCode :: CTree -> String -> (String, String)
toCCode cTree prefix = (toCHeader cTree', toCSource cTree')
    where cTree' = prefixate prefix cTree

-- | Add a prefix to some name.
addPrefix :: String -> String -> String
addPrefix "" s = s
addPrefix prefix s = prefix ++ ('_':s)

instance Prefixable CTree where
    prefixate prefix (CTree (CStruct name) fs) = CTree (CStruct name') fs'
        where
            name' = addPrefix prefix name
            fs' = map (prefixate prefix) fs

instance Prefixable CFunction where
    prefixate prefix (CFunction header instructions) = CFunction header' instructions
        where
            header' = prefixate prefix header

instance Prefixable CFnHeader where
    prefixate prefix (CFnHeader typ name varDecls) = CFnHeader typ' name' varDecls'
        where
            typ' = prefixate prefix typ
            name' = addPrefix prefix name
            varDecls' = map (prefixate prefix) varDecls

instance Prefixable CType where
    prefixate prefix (CUserT name) = CUserT (addPrefix prefix name)
    prefixate prefix (CPtrT typ) = CPtrT (prefixate prefix typ)
    prefixate prefix (CConstT typ) = CConstT (prefixate prefix typ)
    prefixate _ typ = typ

instance Prefixable CVarDecl where
    prefixate prefix (CVarDecl typ name) = CVarDecl typ' name
        where typ' = prefixate prefix typ

-- | Get the C code for a C header file.
-- The function is not yet fully implemented.
-- FIXME: textify functions
toCHeader :: CTree -> String
toCHeader (CTree cStruct _) = intercalate "\n" (stringify cStruct) ++ "\n"
-- TODO: implement (include guard, functions declarations)
-- TODO: optimize

-- | Get the C code for a C source file.
-- The function is not yet fully implemented.
toCSource :: CTree -> String
toCSource (CTree cStruct cFns) =
    intercalate "\n" (intercalate ["\n"] $ map stringify cFns) ++ "\n"
-- TODO: implement
