-- | Translator module containing all translation functionality
module Translator (
    translate
    ) where

import CTreeGen
import Lex
import Synt
import Texter

-- TODO: use some special error type instead of String
-- | Translate Parsiuk code to C header code and C source code.
translate :: String -> Either String (String, String)
translate pSource = alexScanTokens pSource >>= synt >>= toCTree >>=
                    (return . toCCode)
-- TODO: rename alexScanTokens and synt functions and their modules
