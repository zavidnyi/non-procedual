import Data.List

class Foldable m => IntMap m where
    makeMap :: [t] -> m t
    get :: m t -> Int -> t
    update :: m t -> Int -> t -> m t

data TMap t = Empty | Node (TMap t) (Int, t) (TMap t) deriving(Show)

makeMapHelp :: [(Int, t)] -> TMap t
makeMapHelp [] = Empty
makeMapHelp list =
    let
        len = length list
        root = list !! (len `div` 2)
        l = take (len `div` 2) list
        r = take (if odd len then len `div` 2 else len `div` 2 - 1) (reverse list)
    in Node (makeMapHelp l) root (makeMapHelp r)

instance Foldable TMap where
    foldr f z Empty = z
    foldr f z (Node l k r) = foldr f (f (snd k) (foldr f z r)) l

instance IntMap TMap where
    makeMap l = makeMapHelp  [ (i, l !! i) | i <- take (length l) [0..] ]
    get Empty _ = error "Index out of bounds"
    get (Node l (ci, n) r) i
        | ci == i = n
        | otherwise =
            if i > ci then get r i else get l i
    update Empty _ _ = error "Index out of bounds"
    update (Node l (ci, cn) r) i  n 
        | ci == i = Node l (i, n) r
        | otherwise =
            if i > ci then Node (update r i n) (ci, cn) l else Node r (ci, cn) (update l i n)


-- makeMap [1, 2, 3, 8] :: TMap Int
-- makeMap [1, 2, 2, 3] :: TMap Int