import Data.Char (toLower)
import Data.List

lowerString :: String -> String 
lowerString = map toLower 

numberOfPatternsInLine :: String -> String -> Int 
numberOfPatternsInLine line pattern = 
    length [ x| x <- tails line, pattern `isPrefixOf` x]

lookForPatternInText :: [String] -> String -> Int 
lookForPatternInText text pattern = sum [ numberOfPatternsInLine x pattern | x <- text ]


elect :: [[String]] -> [String]
elect table
    | null (concat table) = []
    | otherwise = 
        let
            candidates = nub [ x | x <- concat table ]
            first_votes = [ (candidate, lookForPatternInText (head  (transpose table)) candidate) | candidate <- candidates]
            fewest_voices = minimumBy (\c1 c2 -> compare ( snd c1) (snd c2)) first_votes
            updated_table = [ delete (fst fewest_voices) row | row <- table ]
        in fst fewest_voices : elect updated_table

test1 = elect [
  [ "red", "green" ],
  [ "blue" ],
  [ "green", "yellow", "red", "blue" ],
  [ "blue", "green", "yellow", "red" ],
  [ "green" ]
  ]

test2 = elect [
  [ "karel", "lucie" ],
  [ "karel", "petra", "ondrej" ],
  [ "petra" ],
  [ "petra" ],
  [ "karel", "david", "petra" ],
  [ "petra" ],
  [ "lucie", "karel", "ondrej" ],
  [ "david", "karel" ],
  [ "david", "petra", "ondrej" ],
  [ "david", "lucie" ],
  [ "karel" ],
  [ "lucie" ],
  [ "petra", "ondrej", "lucie" ]
  ]

test3 = elect [
  [ "purple" ],
  [ "orange", "red", "purple" ],
  [ "blue", "orange" ],
  [ "purple", "orange" ],
  [ "red", "purple", "orange" ],
  [ "red", "blue" ],
  [ "red" ],
  [ "orange" ],
  [ "purple", "red", "blue" ],
  [ "red", "orange", "purple", "blue" ],
  [ "orange", "red", "purple" ],
  [ "red", "blue" ],
  [ "red", "purple" ],
  [ "blue" ],
  [ "purple" ],
  [ "orange", "red" ],
  [ "orange", "purple", "blue", "red" ],
  [ "red" ],
  [ "blue", "orange" ],
  [ "red" ],
  [ "purple", "blue" ],
  [ "purple", "blue" ],
  [ "blue", "red", "orange" ],
  [ "red", "purple", "blue" ]
  ]