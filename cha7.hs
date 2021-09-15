mTh :: Num a => a -> a -> a -> a
mTh x = \y -> \z -> x * y * z

addOneIfOdd :: Integer -> Integer
addOneIfOdd n = 
    case odd n of 
        True -> f'' n
        False -> n
    where f'' = \m -> m + 1

--k (x, y) = x 

--k1 :: Integer
--k1 = k ((4-1), 10)

--k2 :: String
--k2 = k ( "three", (1 +2))

--k3 :: Integer
--k3 = k (3, True)

f :: (a, b, c) -> (d, e, f') -> ((a,d), (c, f'))
f (a, _, c) (d, _, f') = ((a, d), (c, f'))

nums :: (Num a, Ord a) => a -> a
nums x =
    case compare x 0 of
        LT -> -1
        GT -> 1
        EQ -> 0

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x 
    | y>=0.9 ='A' 
    | y>=0.7 ='C' 
    | y>=0.8 ='B' 
    | y >= 0.59 = 'D' 
    |otherwise ='F' 
    where y = x / 100

pal :: Eq a => [a] -> Bool
pal xs
    | xs == reverse xs = True
    | otherwise = False

t :: Ord a => a -> a -> Bool
t a b = a > b

--f'' :: a -> a
--f'' a = a

foldBool3 :: a -> a -> Bool -> a
foldBool3 a b bool
    | bool = a
    | otherwise = b

g :: (a -> b) -> (a, c) -> (b, c)
g ab (a, c) =
    (ab a, c)