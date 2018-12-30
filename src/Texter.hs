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

-- | Translate C AST to C code (.h file and .c file).
toCCode :: CTree -> (String, String)
toCCode cTree = (toCHeader cTree, toCSource cTree)

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
