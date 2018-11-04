-- | Translator module containing all translation functionality
module Translator (
    translate
    ) where

import Control.Exception (IOException)

-- TODO
-- | Translate Parsiuk code to C header code and C source code.
-- The function is not yet implemented.
translate :: String -> Either IOException (String, String)
translate _ = Right ("Dummy C header code\n", "Dummy C source code\n")