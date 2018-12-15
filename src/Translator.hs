-- | Translator module containing all translation functionality
module Translator (
    translate
    ) where

-- TODO: use some special error type instead of String
-- TODO: implement
-- | Translate Parsiuk code to C header code and C source code.
-- The function is not yet implemented.
translate :: String -> Either String (String, String)
-- translate _ = Right ("Dummy C header code\n", "Dummy C source code\n")
translate _ = Left "The function 'translate' is not yet implemented" -- FIXME: get the function name
