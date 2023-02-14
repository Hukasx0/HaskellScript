module FromHaskell where

import Text.Parsec
import Text.Parsec.String
import qualified Control.Applicative as A

data Vals = Strj String | Intj String | Charj Char | Boolj String | Letterj String | Mathj Vals String Vals | IfThenElse Vals Vals Vals | Array [Vals] | Param String
            | Map Vals Vals Vals | Length Vals | Filter Vals Vals Vals | Lines Vals | Words Vals | Guards [(Vals, Vals)] | Reverse Vals | Sort Vals | Head Vals | Tail Vals
            | Brackets Vals | Last Vals | Unlines Vals | Unwords Vals | Mappend Vals Vals | Appo Vals | Subs Vals String | JsVal String | Take String Vals | Sum Vals | LF String Vals
            deriving(Eq,Show)

type FunName = String


data Token = Func FunName [Vals] Vals | JsFun String [Vals] | Print Vals | Err Vals | Mapm_ Vals Token Vals | GuardsFunc FunName [Vals] Vals | Let String Vals | JsCode String | LineComment String | Comment String
            | JsLComment String | JsComment String | Include String | JsImport String
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

letterFunParser :: Parser Vals
letterFunParser = LF <$> (many1 letter) <*> (many1 space >> appoParser)

arrayParser :: Parser Vals
arrayParser = Array <$> (char '[' >> spaces >> (valsParser `sepBy` (char ',')) <* spaces <* char ']')

paramParser :: Parser Vals
paramParser = Param <$> many1 letter

mathParser :: Parser Vals
mathParser = Mathj <$> (try strParser <|> try intParser <|> try charParser <|> try boolParser <|>try arrayParser<|>try letterParser <|> bracketsParser) <*> (spaces >> (try (string "++")<|> try (string "+") <|>try (string "-") <|>try (string "*") <|>try (string "/") <|>try (string "==") <|>try (string "/=") <|> try (string ">") <|> try (string "<") <|> try (string "<=") <|> try (string ">=")) <* spaces) <*> (try strParser <|> try intParser <|> try charParser <|> try boolParser <|>try arrayParser<|>try letterParser <|> bracketsParser)

jsFunParser :: Parser Token
jsFunParser = JsFun <$> (string "js{" >> spaces >> many1 (noneOf " ")) <*>  (spaces >> (valsParser `sepBy` (char ' ')) <* spaces <* char '}')

ifThenElseParser :: Parser Vals
ifThenElseParser = IfThenElse <$> (string "if" >> spaces >> valsParser) <*> (spaces >> string "then" >> spaces >> valsParser) <*> (spaces >> string "else" >> spaces >> valsParser)

valsParser :: Parser Vals
valsParser =try mathParser <|> try subsParser <|> try ifThenElseParser <|> try mapParser<|>try appoParser<|>try mappendParser <|>try jsValParser<|> try filterParser <|>try headParser<|> try tailParser<|>try takeParser<|>try lastParser<|>try linesParser<|>try unlinesParser<|>try sumParser<|>try unwordsParser<|> try wordsParser<|>try lengthParser <|> try reverseParser <|> try sortParser <|> try strParser <|>try bracketsParser<|> try intParser <|> try charParser <|> try boolParser <|> try letterFunParser<|>try letterParser <|>arrayParser

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
guardsParser = Guards <$> (char '|' >> ((A.liftA2 (,) (spaces >> ((Letterj <$> (string "otherwise")) <|> valsParser)) (spaces >> char '=' >> spaces >> valsParser)) `sepBy` (string " |")))

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

letParser :: Parser Token
letParser = Let <$> (string "let" >> spaces >> many1 letter) <*> (spaces >> char '=' >> spaces >> valsParser)

lastParser :: Parser Vals
lastParser = Last <$> (string "last" >> many1 space >> valsParser)

unlinesParser :: Parser Vals
unlinesParser = Unlines <$> (string "unlines" >> many1 space >> valsParser)

unwordsParser :: Parser Vals
unwordsParser = Unwords <$> (string "unwords" >> many1 space >> valsParser)

mappendParser :: Parser Vals
mappendParser = Mappend <$>  (try strParser <|> try intParser <|> try charParser <|> try boolParser <|>try arrayParser<|>try letterParser <|> bracketsParser) <*> (many1 space >> string "<>" >> many1 space >>  (try strParser <|> try intParser <|> try charParser <|> try boolParser <|>try arrayParser<|>try letterParser <|> bracketsParser))

appoParser :: Parser Vals
appoParser = Appo <$> (char '$' >> spaces >> valsParser)

subsParser :: Parser Vals
subsParser = Subs <$> (try strParser <|>try arrayParser<|>try letterParser <|> bracketsParser) <*> (spaces >> string "!!" >> spaces >> many1 digit)

takeParser :: Parser Vals
takeParser = Take <$> (string "take" >> many1 space >> many1 digit) <*> (many1 space >> valsParser)

jsCodeParser :: Parser Token
jsCodeParser = JsCode <$> (string "${" >> many1 (noneOf "\\") <* string "\\};")

jsValParser :: Parser Vals
jsValParser = JsVal <$> (string "${" >> many1 (noneOf "\\") <* string "\\};")

lineCommentParser :: Parser Token
lineCommentParser = LineComment <$> (string "--" >> many (noneOf "\n"))

jsLCommentParser :: Parser Token
jsLCommentParser = JsLComment <$> (string "//" >> many (noneOf "\n"))

commentParser :: Parser Token
commentParser = Comment <$> ( between (string "{-") (string "-}") (many (noneOf "-}")) )

jsCommentParser :: Parser Token
jsCommentParser = JsComment <$> ( between (string "/*") (string "*/") (many (noneOf "*/")) )

includeParser :: Parser Token
includeParser = Include <$> (string "include" >> many1 space >> (many (noneOf "\n")))

jsImportParser :: Parser Token
jsImportParser = JsImport <$> ( between (string "<~~~~~>") (string "<~~~~~/>") (many (noneOf "~")) )

sumParser :: Parser Vals
sumParser = Sum <$> (string "sum" >> many1 space >> valsParser)
