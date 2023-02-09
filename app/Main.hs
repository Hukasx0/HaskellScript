module Main where

import Text.Parsec

import FromHaskell
import ToJs

fakeFile :: String
fakeFile = "wordList = [\"Hello\",\"world\",\"this\",\"is\",\"example\",\"code\"]\nmapVals = map (\\word -> word++\"!!!!!\\n\") wordList\nmapM_ (\\mod -> print mod) mapVals\n"

main :: IO ()
main = do
    putStrLn $ fakeFile
    let parsed = parse (spaces >> many ((try funcParser <|>try jsFunParser <|> try printParser <|>try errorParser <|> mapmParser) <* many1 space) <* eof) "test" fakeFile
    print $ parsed
    case parsed of
        Left err -> (print err)
        Right corr -> do
                        let buff = map (hsToJs) corr
                        print $ buff
                        writeFile "out.js" (bufferToJs $ buff)
