-- | Utility module containing functions for working with strings, filenames etc.
module Utilities (
    makeCFileNames
    ) where

import System.FilePath (addExtension, splitExtension)

-- | Make C header and source file names from Parsiuk file name by changing
-- .prl extension to .h and .c; if Parsiuk file has .h or .c extension, .h or .c
-- are changed to _h or _c and then .h and .c extensions added
makeCFileNames :: FilePath -> (FilePath, FilePath)
makeCFileNames pSouceName =
    if extension == ".h" || extension == ".c"
    then (makeMangledFileName "h", makeMangledFileName "c")
    else (makeFileName "h", makeFileName "c")
    where
        (baseFileName, extension) = splitExtension pSouceName
        mangledFileName = baseFileName ++ "_" ++ tail extension
        makeFileName = addExtension baseFileName
        makeMangledFileName = addExtension mangledFileName