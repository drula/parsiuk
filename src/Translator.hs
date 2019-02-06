-- | Translator module containing all translation functionality
module Translator (
    translate
    ) where

import CTreeGen
import Lex
import Synt
import Texter

-- TODO: use some special error type instead of String
-- | Translate Parsiuk code to C header code and C source code
-- with prefix addition.
translate :: String -> String -> Either String (String, String)
translate pSource prefix = toTokens pSource >>= toPTree >>= toCTree >>=
                           (return . (\tree -> toCCode tree prefix))
