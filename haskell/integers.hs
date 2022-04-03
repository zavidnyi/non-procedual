import Data.List ( nub )

-- produces a list of an integer's prime factors in increasing order. Repeated factors should appear multiply in the list.
factors :: Integer -> [Integer]
factors x = factorize x 2

factorize :: Integer -> Integer -> [Integer]
factorize 1 _ = []
factorize num fac | fac * fac > num = [num]
factorize num 2 = if even num then 2 : factorize (num `div` 2) 2 else factorize num 3
factorize num fac = if num `rem` fac == 0 then fac : factorize (num `div` fac) fac else factorize num (fac+2)


-- nub returns only unique elements of the list
squareFree :: [Integer]
squareFree = [x | x <- [1..], let f = factors x in length f == length (nub f)]

squareRoot :: Integer -> Integer
squareRoot x | x <= 1 = x
squareRoot x 
    | x < 0 = error "Negative number"
    | otherwise = binSearch 1 x x

binSearch :: Integer -> Integer -> Integer -> Integer
binSearch low high target 
    | high - low <= 1 = low 
    | otherwise =
        let mid = (high + low) `div` 2
        in case compare (mid * mid) target of
            EQ -> mid 
            LT -> binSearch mid high target
            GT -> binSearch low mid target