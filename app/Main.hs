module Main where

import Control.Exception (IOException, try)
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)

import Translator -- FIXME: rename

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> showUsage
        (pSouceName : _) -> do
            readPSourceResult <- try (readFile pSouceName)
            result <- writeFiles (translate' readPSourceResult) (cHeaderName, cSourceName)
            case result of
                Right () -> do
                    putStrLn $ "Files " ++ cHeaderName ++ " and " ++ cSourceName ++
                        " are successfully created!"
                Left e -> do
                    hPutStrLn stderr $ "Input/output exception:"
                    hPutStrLn stderr $ show e

    where
        showUsage = do
            progName <- getProgName
            putStrLn $ "Usage: " ++ progName ++ " <parsiuk file>"
            -- TODO: allow to pass multiple file arguments

        -- TODO
        cHeaderName = "dummy.h"
        cSourceName = "dummy.c"

-- TODO: add additional info

-- WRITEME: docs
translate' :: Either IOException String -> Either IOException (String, String)
translate' (Right pSource) = Right $ translate pSource
translate' (Left e) = Left e

-- WRITEME: docs
-- FIXME: pass file name as a parameter
-- FIXME: looks ugly
writeFiles :: Either IOException (String, String) -> (FilePath, FilePath) -> IO (Either IOException ())
writeFiles (Left e) _ = return (Left e)
writeFiles (Right (cHeader, cSource)) (cHeaderName, cSourceName) = do
    headerWriteRes <- try $ writeFile cHeaderName cHeader
    case headerWriteRes of
        Right () -> do
            sourceWriteRes <- try $ writeFile cSourceName cSource
            case sourceWriteRes of
                Right () -> return (Right ())
                Left e -> return (Left e)

        Left e -> return (Left e)
