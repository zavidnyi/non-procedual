data Tree = Nil | Node Tree Int Tree
  deriving (Eq, Ord, Show)



allRoots :: [Int] -> [Int]
allRoots list 
    | null list = []
    | length list == 1 = [head list]
    | otherwise = 
        if odd l then [list !! mid] else [list !! (mid-1), list !! mid] 
        where
            l = length list
            mid = l `div` 2

allBalanced :: Int -> [Tree] 
allBalanced = allBalancedInRange 1

allBalancedInRange :: Int -> Int -> [Tree]
allBalancedInRange l h
    | h < l = [Nil]
    | h == l = [Node Nil h Nil]
    | otherwise =
        let list = [l..h] 
        in [Node le root r | root <- allRoots list, le <- allBalancedInRange l (root-1), r <-allBalancedInRange (root+1) h]

test1 = allBalanced 2
test2 = allBalanced 3
test3 = mapM_ print (allBalanced 4)