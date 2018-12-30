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
translate pSource = toTokens pSource >>= toPTree >>= toCTree >>=
                    (return . toCCode)
