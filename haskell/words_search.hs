import Data.Char (toLower)
import Data.List

lowerString :: String -> String 
lowerString = map toLower 

numberOfPatternsInLine :: String -> String -> Int 
numberOfPatternsInLine line pattern = 
    length [ x| x <- tails line, pattern `isPrefixOf` x] + 
    length [ x| x <- tails (reverse line), pattern `isPrefixOf` x ]

lookForPatternInText :: [String] -> String -> Int 
lookForPatternInText text pattern = sum [ numberOfPatternsInLine x pattern | x <- text ]


search :: [String] -> [String] -> [Int]
search input_text input_patterns = 
    let
        patterns = map lowerString input_patterns
        text = map lowerString input_text
        transposed_text = transpose text
    in [ lookForPatternInText text pattern + lookForPatternInText transposed_text pattern | pattern <- patterns]


test = search
        ["ARTTEEB",
         "DSOPNLT",
         "TURNIPA",
         "XKROFET",
         "SKALEOT",
         "MRCTEEB"]
        ["beet", "carrot", "kale", "turnip", "potato"]

test2 = search
        ["ABCBA",
         "BCABC",
         "CABCA",
         "BCABC",
         "ABABA"]
        ["abc", "bca"]


test3 = let line = cycle "dandelion"
            grid = take 400 [take 400 s | s <- tails line]
        in search grid ["Dand", "LION", "iled", "noile", "dandy"]