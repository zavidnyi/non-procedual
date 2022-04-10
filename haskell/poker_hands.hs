import Data.List

numberOfOccurences :: Eq a => a -> [a] -> Int 
numberOfOccurences e list 
    | null list = 0
    | head list == e = 1 + numberOfOccurences e (tail list)
    | otherwise = numberOfOccurences e (tail list)

getSortedFrequences :: Ord a => [a] -> [(a, Int)]
getSortedFrequences list = sortBy (\e1 e2 ->  let ord = compare (snd e2) (snd e1) in if ord == EQ then compare (fst e2) (fst e1) else ord) [ (e, numberOfOccurences e list ) | e <- nub list]

compareFrequences :: Ord a => [(a, Int)] -> [(a, Int)] -> Ordering 
compareFrequences f1 f2
    | null f1 && not (null f2) = LT 
    | not (null f1) && null f2 = GT
    | null f1 && null f2 = EQ 
    | otherwise =
        let 
            e1 = head f1
            e2 = head f2
        in case compare (snd e1) (snd e2) of
            GT -> GT
            LT -> LT 
            EQ -> let ord = compare (fst e1) (fst e2) in if ord == EQ then compareFrequences (tail f1) (tail f2) else ord


hasNOfAKind :: Int -> [(a, Int)] -> Bool 
hasNOfAKind _ [] = False 
hasNOfAKind n hand = snd (head hand) >= n

hasFourOfAKind :: [(Int, Int)] -> Bool 
hasFourOfAKind = hasNOfAKind 4 

hasFullHouse :: [(Int, Int)] -> Bool 
hasFullHouse hand = hasNOfAKind 3 hand && hasNOfAKind 2 (tail hand)

hasThreeOfAKind :: [(Int, Int)] -> Bool 
hasThreeOfAKind = hasNOfAKind 3 

hasTwoPairs :: [(Int, Int)] -> Bool 
hasTwoPairs hand = hasNOfAKind 2 hand && hasNOfAKind 2 (tail hand)

hasOnePair :: [(Int, Int)] -> Bool 
hasOnePair = hasNOfAKind 2

hasHighCard :: [(Int, Int)] -> Bool 
hasHighCard [] = False 
hasHighCard hand = fst (maximumBy (\e1 e2 -> compare (fst e2) (fst e1)) hand) >= 11 


getRankOfHand :: [(Int, Int)] -> Int
getRankOfHand [] = 0
getRankOfHand hand
    | hasFourOfAKind hand = 6
    | hasFullHouse hand = 5
    | hasThreeOfAKind hand = 4
    | hasTwoPairs hand = 3
    | hasOnePair hand = 2
    | hasHighCard hand = 1
    | otherwise = 0


better :: [Int] -> [Int] -> Bool
better h1 h2 
    | length h1 < 5 || length h2 < 5 = error "Invalid hand"
    | getRankOfHand (getSortedFrequences h1) > getRankOfHand ( getSortedFrequences h2) = True 
    | getRankOfHand (getSortedFrequences h1) < getRankOfHand ( getSortedFrequences h2) = False
    | otherwise = compareFrequences (getSortedFrequences h1) ( getSortedFrequences h2) == GT


(jack, queen, king, ace) = (11, 12, 13, 14)

test1 = better [7, 7, 7, 2, 2] [5, 5, 5, king, king]
test2 = better [2, jack, 2, jack, 4] [8, 9, 8, 9, 5]
test3 = better [5, 5, 3, 2, 3] [jack, jack, 6, 4, 2]
test4 = better [queen, queen, queen, 5, 2] [9, 9, 9, 4, 4]
test5 = better [2, 4, 6, jack, queen] [3, 4, 5, jack, queen]