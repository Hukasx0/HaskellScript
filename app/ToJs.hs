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
hsToJs _ = ""

valToJs :: Vals -> String
valToJs (Intj i) = i
valToJs (Strj s) = "\""++s++"\""
valToJs (Letterj l) = l++"()"
valToJs (Boolj b) = b
valToJs (Param p) = p
valToJs (Length v) = (valToJs $ v)++".length"
valToJs (Array a) = "["++(intercalate "," (map valToJs a))++"]"
valToJs (IfThenElse condition tru fal) = (valToJs $ condition)++" ? "++(valToJs $ tru)++" : "++(valToJs $ fal)
valToJs (Mathj a "/=" b) = (valToJs $ a)++"!="++(valToJs $ b)  
valToJs (Mathj a "++" b) = (valToJs $ a)++".concat("++(valToJs $ b)++")"
valToJs (Mathj a x b) = (valToJs $ a)++x++(valToJs $ b)
valToJs (Map param op val) = (valToJs $ val)++".map("++(valToJs $ param)++" => "++ (valToJs $ (isParam op param))++")"
valToJs (Filter param op val) = (valToJs $ val)++".filter("++(valToJs $ param)++" => "++ (valToJs $ (isParam op param))++")"
valToJs (Lines s) = (valToJs $ s)++".split(\"\\n\")"
valToJs (Words s) = (valToJs $ s)++".split(\" \")"
valToJs _ = ""

ioParam :: Token -> Vals -> Token
ioParam (Print v) param = Print (isParam v param) 
ioParam (Err v) param = Err (isParam v param) 
ioParam a _ = a

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