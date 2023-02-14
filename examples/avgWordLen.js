const exampleString =  "Hello world! This is example string"

```hs

-- HaskellScript avgWordLen.js
-- HaskellScript.exe avgWordLen.js

splitStr s = words $ s

wordsLen wordList = map (\w -> length $ w) wordList

avgWordLen lenList = (sum $ lenList) / (length $ lenList)

getAvgWordLen s = avgWordLen $ wordsLen $ splitStr $ s

// node avgWordLen.js

```

console.log("Average word length is: "+getAvgWordLen(exampleString));