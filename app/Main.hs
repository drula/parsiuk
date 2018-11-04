-- | Module Main
module Main where

import Control.Exception (IOException, try)
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)

import Translator

-- | The main application function
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> showUsage
        (pSouceName : _) -> do
            result <- try $ readTranslateWrite pSouceName (cHeaderName, cSourceName)
            processResult result

    where
        showUsage = do
            progName <- getProgName
            putStrLn $ "Usage: " ++ progName ++ " <parsiuk file>"
            -- TODO: allow to pass multiple file arguments

        processResult :: Either IOException () -> IO ()
        processResult (Left e) = do
            hPutStrLn stderr $ "Input/output exception:"
            hPutStrLn stderr $ show e
        processResult (Right ()) = return ()

        -- TODO
        cHeaderName = "dummy.h"
        cSourceName = "dummy.c"

-- | Read Parsiuk source code file, translate it and write to C header and source files.
-- Throw IOException in case of IO error.
readTranslateWrite :: FilePath -> (FilePath, FilePath) -> IO ()
readTranslateWrite pSouceName (cHeaderName, cSourceName) = do
    pSource <- readFile pSouceName
    case (translate pSource) of
        Right (cHeader, cSource) -> do
            writeFile cHeaderName cHeader
            writeFile cSourceName cSource
            putStrLn $ "Files " ++ cHeaderName ++
                        " and " ++ cSourceName ++
                        " are successfully created!"
        Left e -> hPutStrLn stderr e
