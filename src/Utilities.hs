-- | Utility module containing functions for working with strings, filenames etc.
module Utilities (
    makeCFileNames
    ) where

import System.FilePath (addExtension, dropExtension)

-- | Make C header and source file names from Parsiuk file name by changing
-- .prl extension to .h and .c
makeCFileNames :: FilePath -> (FilePath, FilePath)
makeCFileNames pSouceName = (cHeaderName, cSourceName)
    where
        cHeaderName = addExtension baseFileName "h"
        cSourceName = addExtension baseFileName "c"
        baseFileName = dropExtension pSouceName
