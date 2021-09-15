import Data.Char

eftBool :: Bool -> Bool -> [Bool]
eftBool True True = [True]
eftBool False False = [False]
eftBool False True = [False, True]
eftBool True False = []

myWords :: String -> [String]
myWords [] = []
myWords (' ':xs) = myWords xs
myWords a =
    let
        headWord = takeWhile (/=' ') a
        rest = dropWhile (/=' ') a
    in
        headWord : myWords  rest

splitOn :: Char -> String -> [String]
splitOn c given =
    mySplit given
    where
        mySplit [] = []
        mySplit (x:xs) = 
            if x == c then
                mySplit xs
            else
                let
                    headWord = takeWhile (/=c) (x:xs)
                    rest = dropWhile (/=c) (x:xs)
                in
                    headWord : mySplit  rest

firstSen = "Tyger Tyger, burning bright\n" 
secondSen = "In the forests of the night\n" 
thirdSen = "What immortal hand or eye\n" 
fourthSen = "Could frame thy fearful\
\ symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines = splitOn '\n'

shouldEqual = [ "Tyger Tyger, burning bright"
                , "In the forests of the night"
                , "What immortal hand or eye"
                , "Could frame thy fearful symmetry?" ]

main :: IO () 
main =
    print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)

myzip :: [a] -> [b] -> [(a, b)]
myzip [] _ = []
myzip _ [] = []
myzip (a:as) (b:bs) =
        (a, b) : zip as bs

myzipwith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipwith _ [] _ = []
myzipwith _ _ [] = []
myzipwith f (a:as) (b:bs) =
        f a b : myzipwith f as bs

myzip' :: [a] -> [b] -> [(a, b)]
myzip' = myzipwith (,)

allUpper xs = filter isUpper xs

cap :: String -> String
cap (x:xs) = toUpper x  : xs

allcap :: String -> String
allcap [] = []
allcap (x:xs) = toUpper x : allcap xs

justcap = toUpper . head