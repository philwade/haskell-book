module ChapterEight where

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b =b
applyTimes n f b =
    f . applyTimes (n - 1) f $ b

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny

appedCatty = cattyConny "whoops"
frappe = flippy "haha"

summer :: (Eq a, Num a) => a -> a
summer n = go 0
    where go acc
            | acc == n = acc
            | otherwise = acc + go (acc + 1)

mult :: (Integral a) => a -> a -> a
mult n times
    | times == 0 = 0
    | otherwise = n + mult n (times - 1)

mc91 n
    | n <= 100 = 91
    | otherwise = n - 10