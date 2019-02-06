-- | Module Main
module Main where

import Control.Exception (IOException, try)
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)

import Translator
import Utilities

-- | The main application function
main :: IO ()
main = do
    args <- getArgs
    let args' = getArgs' args
    case args' of
        Just (pSourceName, prefix) -> do
            result <- try $ readTranslateWrite pSourceName prefix $
                makeCFileNames pSourceName
            processResult result
        Nothing -> showUsage

    where
        showUsage = do
            progName <- getProgName
            putStrLn $ "Usage: " ++ progName ++ " <parsiuk file> [prefix]"
            -- TODO: allow to pass multiple file arguments
            -- TODO: write more verbous help message

        getArgs' (pSourceName : prefix : _) = Just (pSourceName, prefix)
        getArgs' (pSourceName : []) = Just (pSourceName, "")
        getArgs' [] = Nothing

        processResult :: Either IOException () -> IO ()
        processResult (Left e) = do
            hPutStrLn stderr $ "Input/output exception:"
            hPutStrLn stderr $ show e
        processResult (Right ()) = return ()

-- | Read Parsiuk source code file, translate it with adding prefix to
-- C types and function names and write to C header and source files.
-- Throw IOException in case of IO error.
readTranslateWrite :: FilePath -> String -> (FilePath, FilePath) -> IO ()
readTranslateWrite pSourceName prefix (cHeaderName, cSourceName) = do
    pSource <- readFile pSourceName
    case (translate pSource prefix) of
        Right (cHeader, cSource) -> do
            writeFile cHeaderName cHeader
            writeFile cSourceName cSource
            -- TODO: check if cHeaderName and cSourceName files already exist
            putStrLn $ "Files " ++ cHeaderName ++
                        " and " ++ cSourceName ++
                        " are successfully created!"
        Left e -> hPutStrLn stderr $ "Error: " ++ e
