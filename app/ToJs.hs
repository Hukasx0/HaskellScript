module ToJs where

import Data.List
import FromHaskell

bufferToJs :: [String] -> String
bufferToJs b = intercalate "\n" b

hsToJs :: Token -> String
hsToJs (Func name params val) = "const "++name++" = ("++(intercalate ", " (map (\p -> []++(valToJs $ p)) params))++") => "++(valToJs $ (manyParams val params))
hsToJs (JsFun name params) = name++"("++(intercalate ", " (map (\p -> []++(valToJs $ p)) params))++")"
hsToJs (Print v) = "console.log("++(valToJs $ v)++")"
hsToJs (Err v) = "console.error("++(valToJs $ v)++")"
hsToJs (Mapm_ param io val) = (valToJs $ val)++".forEach("++(valToJs $ param)++" => "++(hsToJs $ (ioParam io param))++")"
hsToJs (GuardsFunc name params gs) = "const "++name++" = ("++(intercalate ", " (map (\p -> []++(valToJs $ p)) params))++") => {\n"++(valToJs gs)++"}\n"
hsToJs (Let n v) = "let "++n++" = "++(valToJs $ v)
hsToJs (JsCode code) = code
hsToJs (Comment _) = []
hsToJs (LineComment _) = ""
hsToJs (JsComment c) ="/*"++c++"*/"
hsToJs (JsLComment c) = "//"++c
hsToJs (Include _) = ""
hsToJs (JsImport s) = s
hsToJs _ = []

valToJs :: Vals -> String
valToJs (Intj i) = i
valToJs (Strj s) = "\""++s++"\""
valToJs (Letterj l) = l++"()"
valToJs (Boolj b) = b
valToJs (Param p) = p
valToJs (Length v) = (valToJs $ v)++".length"
valToJs (Subs a i) = (valToJs $ a)++".at("++i++")"
valToJs (Array a) = "["++(intercalate "," (map valToJs a))++"]"
valToJs (IfThenElse condition tru fal) = (valToJs $ condition)++" ? "++(valToJs $ tru)++" : "++(valToJs $ fal)
valToJs (Mathj a "/=" b) = (valToJs $ a)++"!="++(valToJs $ b)  
valToJs (Mathj a "++" b) = (valToJs $ a)++".concat("++(valToJs $ b)++")"
valToJs (Mathj a x b) = (valToJs $ a)++x++(valToJs $ b)
valToJs (Map param op val) = (valToJs $ val)++".map("++(valToJs $ param)++" => "++ (valToJs $ (isParam op param))++")"
valToJs (Filter param op val) = (valToJs $ val)++".filter("++(valToJs $ param)++" => "++ (valToJs $ (isParam op param))++")"
valToJs (Lines s) = (valToJs $ s)++".split(\"\\n\")"
valToJs (Unlines s) = (valToJs $ s)++".join(\"\\n\")"
valToJs (Unwords s) = (valToJs $ s)++".join(\" \")"
valToJs (Mappend x y) = (valToJs $ x)++".concat("++(valToJs $ y)++")"
valToJs (Words s) = (valToJs $ s)++".split(\" \")"
valToJs (Reverse s) = (valToJs $ s)++".reverse()"
valToJs (Sort s) = (valToJs $ s)++".split()"
valToJs (Head s) = (valToJs $ s)++".shift()"
valToJs (Tail s) = (valToJs $ s)++".slice(1)"
valToJs (Last s) = (valToJs $ s)++".slice(-1)"
valToJs (Sum s) = (valToJs $ s)++".reduce((a, b) => a + b, 0)"
valToJs (Take n s) = (valToJs $ s)++".slice(0,"++n++")"
valToJs (Appo s) = (valToJs $ s)
valToJs (Brackets s) = "("++(valToJs $ s)++")"
valToJs (JsVal s) = s
valToJs (LF f v) = f++"("++(valToJs $ v)++")"
valToJs (Guards []) = error $ "syntax error"
valToJs (Guards vals) = "\tif("++(valToJs $ fst $ currGd)++"){\n\t\treturn "++(valToJs $ snd $ currGd)++";\n\t}\n"++(concat $ map getGuard (tail $ vals))
                       where currGd=head $ vals
valToJs _ = ""

ioParam :: Token -> Vals -> Token
ioParam (Print v) param = Print (isParam v param) 
ioParam (Err v) param = Err (isParam v param) 
ioParam a _ = a

getGuard :: (Vals,Vals) -> String
getGuard (Letterj "otherwise",b)= "\telse{\n\t\treturn "++(valToJs $ b)++";\n\t}\n"
getGuard guard= "\telse if("++(valToJs $ fst $ guard)++"){\n\t\treturn "++(valToJs $ snd $ guard)++";\n\t}\n"

manyParams :: Vals -> [Vals] -> Vals
manyParams val [] = val
manyParams val params = manyParams (isParam val (head $ params)) (tail $ params)

isParam :: Vals -> Vals -> Vals
isParam (Letterj a) (Param b)|a==b=(Param a)
                             |otherwise=(Letterj a)
isParam (Mathj a b c) (Param d) = (Mathj (isParam a (Param d)) b (isParam c (Param d)))
isParam (Length a) (Param b) = (Length (isParam a (Param b))) 
isParam (Lines a) (Param b) = (Lines (isParam a (Param b))) 
isParam (Words a) (Param b) = (Words (isParam a (Param b))) 
isParam (Reverse a) (Param b) = (Reverse (isParam a (Param b))) 
isParam (Sort a) (Param b) = (Sort (isParam a (Param b))) 
isParam (Head a) (Param b) = (Head (isParam a (Param b))) 
isParam (Tail a) (Param b) = (Tail (isParam a (Param b))) 
isParam (Last a) (Param b) = (Last (isParam a (Param b))) 
isParam (Sum a) (Param b) = (Sum (isParam a (Param b))) 
isParam (Take c a) (Param b) = (Take c (isParam a (Param b))) 
isParam (Brackets a) (Param b) = (Brackets (isParam a (Param b))) 
isParam (Appo v) (Param b) = (Appo (isParam v (Param b)))
isParam (LF n v) (Param b) = (LF n (isParam v (Param b)))
isParam (Map p o v) (Param b) = (Map p o (isParam v (Param b)))
isParam a _ = a
