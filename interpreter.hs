module Main where

#ifdef USE_HASKELINE
import Control.Monad
import System.Console.Haskeline
import System.FilePath
#endif
import System.Environment
import System.Exit(exitWith, ExitCode(..))
import System.IO

import ErrM
import Parfoo
import Layoutfoo

import Semantics(moduleSem, showSemError)
-- import StdLib

compilationFailure :: IO a
compilationFailure = exitWith $ ExitFailure 3

main :: IO ()
main = do
    args <- getArgs
    (code, programPath) <- case args of
        [] -> do
            code <- readStdin
            return (code, "<stdin>")
        [programPath] -> do
            code <- readFile programPath
            return (code, programPath)
        _ -> do
            progName <- getProgName
            hPutStrLn stderr $ "Usage:\n    " ++ progName ++ " [FILE PATH]\n"
            compilationFailure
    case pModule $ resolveLayout True . myLexer $ code of
        Bad errmsg -> do
            hPutStrLn stderr $ "Parser error:\n" ++ errmsg ++ "\n"
            compilationFailure
        Ok moduleSyntax -> do
            case moduleSem moduleSyntax programPath {- stdlib -} of
                Left errmsg -> do
                    hPutStrLn stderr $ showSemError errmsg
                    compilationFailure
                Right runModule -> runModule
    where
#ifdef USE_HASKELINE
        readStdin = do
            histfile <- do
                liftM (`replaceFileName` ".foo-history") getExecutablePath
            runInputT defaultSettings{historyFile=Just histfile} readLines
            where
                readLines :: InputT IO String
                readLines = do
                    ui <- haveTerminalUI
                    readLines' $ if ui then "> " else ""
                readLines' :: String -> InputT IO String
                readLines' prompt = do
                    maybeLine <- getInputLine prompt
                    case maybeLine of
                        Nothing -> return ""
                        Just ln -> do
                            rest <- readLines' prompt
                            return $ ln ++ "\n" ++ rest
#else
        readStdin = getContents
#endif
