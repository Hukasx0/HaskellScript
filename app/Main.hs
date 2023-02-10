module Main where

import Text.Parsec
import System.FilePath
import System.Environment
import System.Directory

import FromHaskell
import ToJs

compileHs :: String -> FilePath -> IO ()
compileHs fData fName = do
    let parsed = parse (spaces >> many ((try funcParser <|> try printParser <|>try errorParser <|>mapmParser<|>try guardsFuncParser<|> try jsFunParser)  <* spaces) <* eof) fName fData
    print $ parsed
    case parsed of
        Left err -> (print err)
        Right corr -> do
                        let buff = map (hsToJs) corr
                        print $ buff
                        writeFile (fName++".js") (bufferToJs $ buff)

compileJhs :: String -> FilePath -> IO ()
compileJhs fData fName = do
    print $ middle
    let parsed = parse (spaces >> many ((try funcParser <|> try printParser <|>try errorParser <|>mapmParser<|>try guardsFuncParser<|> try jsFunParser) <* spaces) <* eof) fName (concat $ middle)
    print $ parsed
    case parsed of
        Left err -> (print err)
        Right corr -> do
                        let buff = map (hsToJs) corr
                        print $ buff
                        writeFile (fName++".js.tmp") ((bufferToJs $ before)++"\n"++(bufferToJs $ buff)++"\n"++(bufferToJs $ drop 1 after))
                        renameFile (fName++".js.tmp") (fName++".js")
    where (before, rest) = break (== "```hs") (lines $ fData)
          (middle, after) = span (/= "```") (drop 1 rest)

main :: IO ()
main = do
    fName <- head <$> getArgs
    fData <- readFile $ fName
    if (takeExtension $ fName)==".js" then (compileJhs fData (dropExtension $ fName))
    else (compileHs fData (dropExtension $ fName)) 
