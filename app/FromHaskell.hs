module FromHaskell where

import Text.Parsec
import Text.Parsec.String
import qualified Control.Applicative as A

data Vals = Strj String | Intj String | Charj Char | Boolj String | Letterj String | Mathj Vals String Vals | IfThenElse Vals Vals Vals | Array [Vals] | Param String
            | Map Vals Vals Vals | Length Vals | Filter Vals Vals Vals | Lines Vals | Words Vals | Guards [(Vals, Vals)] | Reverse Vals | Sort Vals | Head Vals | Tail Vals
            | Brackets Vals
            deriving(Eq,Show)

type FunName = String


data Token = Func FunName [Vals] Vals | JsFun String [Vals] | Print Vals | Err Vals | Mapm_ Vals Token Vals | GuardsFunc FunName [Vals] Vals
            deriving(Eq,Show)

strParser :: Parser Vals
strParser = Strj <$> (char '"' >> many1 (noneOf "\"") <* char '"')

intParser :: Parser Vals
intParser = Intj <$> (many1 digit)

charParser :: Parser Vals
charParser = Charj <$> (char '\'' >> letter <* char '\'')

boolParser :: Parser Vals
boolParser = Boolj <$> ((string "true") <|> (string "false"))

letterParser :: Parser Vals
letterParser = Letterj <$> (many1 letter)

arrayParser :: Parser Vals
arrayParser = Array <$> (char '[' >> spaces >> (valsParser `sepBy` (char ',')) <* spaces <* char ']')

paramParser :: Parser Vals
paramParser = Param <$> many1 letter

mathParser :: Parser Vals
mathParser = Mathj <$> (try strParser <|> try intParser <|> try charParser <|> try boolParser <|>try arrayParser<|>try letterParser <|> bracketsParser) <*> (spaces >> (try (string "++")<|> try (string "+") <|>try (string "-") <|>try (string "*") <|>try (string "/") <|>try (string "==") <|>try (string "/=") <|> try (string ">") <|> try (string "<") <|> try (string "<=") <|> try (string ">=")) <* spaces) <*> (try strParser <|> try intParser <|> try charParser <|> try boolParser <|>try arrayParser<|>letterParser) 

jsFunParser :: Parser Token
jsFunParser = JsFun <$> (string "js{" >> spaces >> many1 (noneOf " ")) <*>  (spaces >> (valsParser `sepBy` (char ' ')) <* spaces <* char '}')

ifThenElseParser :: Parser Vals
ifThenElseParser = IfThenElse <$> (string "if" >> spaces >> valsParser) <*> (spaces >> string "then" >> spaces >> valsParser) <*> (spaces >> string "else" >> spaces >> valsParser)

valsParser :: Parser Vals
valsParser =try mathParser <|> try ifThenElseParser <|> try mapParser <|> try filterParser <|>try headParser<|> try tailParser<|>try linesParser<|> try wordsParser<|>try lengthParser <|> try reverseParser <|> try sortParser <|> try strParser <|>try bracketsParser<|> try intParser <|> try charParser <|> try boolParser <|>try letterParser <|>arrayParser

funcParser :: Parser Token
funcParser = Func <$> (many1 letter <* spaces) <*> (paramParser `sepBy` (char ',')) <*> (spaces >> char '=' >> spaces >> valsParser)

printParser :: Parser Token
printParser = Print <$> (string "print" >> spaces >> valsParser)

errorParser :: Parser Token
errorParser = Err <$> (string "error" >> spaces >> valsParser)

mapmParser :: Parser Token
mapmParser = Mapm_ <$> (string "mapM_" >> spaces >> char '(' >> char '\\' >> paramParser) <*> (spaces >> string "->" >> spaces  >> (try jsFunParser <|> try printParser <|>try errorParser) <* spaces <* char ')' <* spaces) <*> valsParser

mapParser :: Parser Vals
mapParser = Map <$> (string "map" >> spaces >> char '(' >> char '\\' >> paramParser) <*> (spaces >> string "->" >> spaces  >> valsParser <* spaces <* char ')' <* spaces) <*> valsParser

filterParser :: Parser Vals
filterParser = Filter <$> (string "filter" >> spaces >> char '(' >> char '\\' >> paramParser) <*> (spaces >> string "->" >> spaces  >> valsParser <* spaces <* char ')' <* spaces) <*> valsParser

lengthParser :: Parser Vals
lengthParser = Length <$> (string "length" >> many1 space >> valsParser)

linesParser :: Parser Vals
linesParser = Lines <$> (string "lines" >> many1 space >> valsParser)

wordsParser :: Parser Vals
wordsParser = Words <$> (string "words" >> many1 space >> valsParser)

guardsFuncParser :: Parser Token
guardsFuncParser = GuardsFunc <$> (many1 letter <* spaces) <*> (paramParser `sepBy` (char ',')) <*> (spaces >> guardsParser)

guardsParser :: Parser Vals
guardsParser = Guards <$> ((A.liftA2 (,) (spaces >> char '|' >> spaces >> ((Letterj <$> (string "otherwise")) <|> valsParser)) (spaces >> char '=' >> spaces >> valsParser)) `sepBy` (string " &"))

reverseParser :: Parser Vals
reverseParser = Reverse <$> (string "reverse" >> many1 space >> valsParser)

sortParser :: Parser Vals
sortParser = Sort <$> (string "sort" >> many1 space >> valsParser)

headParser :: Parser Vals
headParser = Head <$> (string "head" >> many1 space >> valsParser)

tailParser :: Parser Vals
tailParser = Tail <$> (string "tail" >> many1 space >> valsParser)

bracketsParser :: Parser Vals
bracketsParser = Brackets <$> (char '(' >> spaces >> valsParser <* spaces <* char ')')
