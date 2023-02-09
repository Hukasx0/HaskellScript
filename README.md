# HaskellScript
Simple Haskell -> JavaScript compiler
## example
This Haskell code:
```Haskell
wordList = ["Hello","world","this","is","example","code"]
mapVals = map (\word -> word++"!!!!!\n") wordList
mapM_ (\mod -> print mod) mapVals
```
compiles to this JavaScript code:
```Javascript
const wordList = () => ["Hello","world","this","is","example","code"]
const mapVals = () => wordList().map(word => word.concat("!!!!!\n"))
mapVals().forEach(mod => console.log(mod))
```
