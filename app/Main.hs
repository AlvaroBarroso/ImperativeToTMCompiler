module Main where

import System.Environment (getArgs)
import SimpleImperativeLanguage.Par (pComm, myLexer)
import SimpleImperativeLanguage.Print (printTree)
import Semantic.Checker (semanticCheck)

-- Function to read file content
readFileContent :: FilePath -> IO String
readFileContent = readFile

-- Function to print detailed error messages
printErrors :: [String] -> IO ()
printError = map printError

printError :: String -> IO ()
printError err = putStrLn $ "Syntax error: " ++ err


-- Main function
main :: IO ()
main = do
    -- Get the file path from command line arguments
    args <- getArgs
    let filePath = head args

    -- Read the file content
    content <- readFileContent filePath

    -- Lex the content
    let tokens = myLexer content

    -- Parse the tokens
    case pComm tokens of
        Left parseErr -> printError parseErr
        Right parsed -> case semanticCheck parsed of
            Maybe semErr -> printErrors semErr
            Nothing -> putStrLn "Parsed Successfully"
            -- Nothing -> putStrLn $ printTree parsed