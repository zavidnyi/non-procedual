import Data.List

isReflexive :: (a -> a -> Bool) -> [a] -> Bool
isReflexive _ [] = True
isReflexive rel set = rel e e && isReflexive rel (tail set) where e = head set

isSymmetric :: (a -> a -> Bool) -> [a] -> Bool
isSymmetric rel set = nub [ rel e2 e1 | e1 <- set, e2 <- set, rel e1 e2] == [True]

isTransitive :: (a -> a -> Bool) -> [a] -> Bool
isTransitive rel set = nub [ rel e1 e3 | e1 <-set, e2 <- set, e3 <- set, rel e1 e2 && rel e2 e3] == [True]

isEquiv :: (a -> a -> Bool) -> [a] -> Bool
isEquiv rel set = isReflexive rel set && isSymmetric rel set && isTransitive rel set

is_equiv :: (a -> a -> Bool) -> [a] -> Bool
is_equiv = isEquiv

eqtest1 = is_equiv (\x y -> x `mod` 5 == y `mod` 5) [1..30]
eqtest2 = is_equiv (<=) [1..30]
eqtest3 = is_equiv (\x y -> False) [1..3]
eqtest4 = is_equiv (\i j -> abs(i - j) <= 2) [1..20]


getClassOf :: a -> (a -> a -> Bool) -> [a] -> [a]
getClassOf _ _ [] = []
getClassOf e rel set = 
    if rel e en then en : getClassOf e rel (tail set)
    else getClassOf e rel (tail set)
    where en = head set

classes :: (a -> a -> Bool) -> [a] -> [[a]]
classes _ [] = []
classes rel set = c : classes rel (snd (partition (\a -> rel a (head c)) set)) where c = getClassOf (head set) rel set

reflexiveClosure :: Eq a => (a -> a -> Bool) -> (a -> a -> Bool)
reflexiveClosure rel e1 e2 = e1 == e2 || rel e1 e2

reflexive_closure :: Eq a => (a -> a -> Bool) -> (a -> a -> Bool)
reflexive_closure = reflexiveClosure