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

-- | Translate C AST to C code (.h file and .c file) with prefix addition.
toCCode :: CTree -> String -> (String, String)
toCCode cTree prefix = (toCHeader cTree', toCSource cTree')
    where cTree' = prefixate cTree prefix

-- | Add prefix to types and functions.
-- The function is not yet fully implemented.
prefixate :: CTree -> String -> CTree
prefixate (CTree (CStruct name)) prefix = CTree (CStruct name')
    where name' = if null prefix then name else prefix ++ "_" ++ name
-- TODO: probably it needs to create Prefixable class

-- | Get the C code for a C header file.
-- The function is not yet fully implemented.
toCHeader :: CTree -> String
toCHeader (CTree cStruct) = (intercalate "\n" $ stringify cStruct) ++ "\n"
-- TODO: implement (include guard, functions declarations)
-- TODO: optimize

-- | Get the C code for a C source file.
-- The function is not yet fully implemented.
toCSource :: CTree -> String
toCSource _ = "Dummy C source code\n"
-- TODO: implement
