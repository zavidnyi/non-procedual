import Data.Maybe
import Data.List

type Graph = [(Int, [Int])]

g :: [(Int, [Int])]
g = [ (1, [2, 3, 4]), (2, [1, 5]), (3, [1, 5]), (4, [1, 5]), (5, [3, 4]) ]

data Color = Red | Black
    deriving (Eq, Show)


bicolor :: Graph -> Maybe [ (Int, Color) ]
bicolor g = bfs g [(1, Red)] [1]
-- bicolorHelp ((V, adj): rest) coloring =


adjacent :: Graph -> Int -> [Int]
adjacent graph v = fromJust (lookup v graph)

bfs :: [(Int, [Int])] -> [(Int, Color)] -> [Int] -> Maybe [(Int, Color)]
bfs g visited [] = Just visited
bfs g visited (v : vs) =
    let ns = adjacent g v
    in 
        if sum [ if  (lookup n visited) == (lookup v visited) then 1 else 0 | n <- ns, c <- visited] > 0 then Nothing
        else bfs g ([ (n, if fromJust (lookup v visited) == Red then Black else Red) | n <-ns] ++ visited) (vs ++ ns)
