import Data.Char

join :: String -> [String] -> String
join _ [] = ""
join sep [str] = str
join sep (str:rest) = str ++ sep ++ join sep rest


capitalize :: String -> String
capitalize [] = ""
capitalize (' ':str) = ' ' : capitalize str
capitalize (c:str) =
    let (word, rest) = getWord str
    in toUpper c : word ++ capitalize rest

getWord :: String -> (String, String)
getWord [] = ("", "")
getWord (c:str) 
    | c == ' ' = ("", c:str)
    | otherwise = 
        let (word, rest) = getWord  str
        in (c:word,rest)

strWords :: String -> [String]
strWords "" = []
strWords (' ':str) = strWords str
strWords (c:str) = 
    let (word, rest) = getWord str
    in (c:word) : strWords rest
