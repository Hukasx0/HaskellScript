module ToJs where

import Data.List
import FromHaskell

bufferToJs :: [String] -> String
bufferToJs b = intercalate "\n" b

hsToJs :: Token -> String
hsToJs (Func name params val) = "const "++name++" = ("++(intercalate ", " (map (\p -> []++(valToJs $ p)) params))++") => "++(valToJs $ val)
hsToJs (JsFun name params) = name++"("++(intercalate ", " (map (\p -> []++(valToJs $ p)) params))++")"
hsToJs (Print v) = "console.log("++(valToJs $ v)++")"
hsToJs (Err v) = "console.error("++(valToJs $ v)++")"
hsToJs (Mapm_ param io val) = (valToJs $ val)++".forEach("++(valToJs $ param)++" => "++(hsToJs $ (ioParam io param))++")"
hsToJs (GuardsFunc name params gs) = "const "++name++" = ("++(intercalate ", " (map (\p -> []++(valToJs $ p)) params))++") => {\n"++(valToJs gs)++"}\n"
hsToJs (Let n v) = "let "++n++" = "++(valToJs $ v)
hsToJs (JsCode code) = code
hsToJs (Comment _) = ""
hsToJs (LineComment _) = ""
hsToJs (JsComment c) ="/*"++c++"*/"
hsToJs (JsLComment c) = "//"++c
hsToJs (Include _) = ""
hsToJs (JsImport s) = s
hsToJs _ = ""

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
valToJs (Take n s) = (valToJs $ s)++".slice(0,"++n++")"
valToJs (Appo s) = (valToJs $ s)
valToJs (Brackets s) = "("++(valToJs $ s)++")"
valToJs (JsVal s) = s
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

isParam :: Vals -> Vals -> Vals
isParam (Mathj (Letterj a) b c) (Param d)|a==d=(Mathj (Param a) b c)
                                     |otherwise=(Mathj (Letterj a) b c)
isParam (Mathj a b (Letterj c)) (Param d)|c==d=(Mathj a b (Param c))
                                     |otherwise=(Mathj a b (Letterj c))
isParam (Letterj a) (Param b)|a==b=(Param a)
                             |otherwise=(Letterj a)
isParam (Length (Letterj a)) (Param b)|a==b=(Length (Param a))
                                      |otherwise=(Length (Letterj a))
isParam (Lines (Letterj a)) (Param b)|a==b=(Lines (Param a))
                                      |otherwise=(Lines (Letterj a))
isParam (Words (Letterj a)) (Param b)|a==b=(Words (Param a))
                                      |otherwise=(Words (Letterj a))
isParam a _ = a
