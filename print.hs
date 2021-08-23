module Print1 where

main :: IO ()
main = do
    putStrLn "lets gooo"
    putStr "to mars"
    putStrLn $ "forreeever" ++ "eerrrrrrr"
    putStrLn etc
    where
        etc :: String
        etc = concat ["etc ", "so on ", "and general", orr]

orr :: String
orr = "or whatevers"

area d = pi * (r * r)
        where
            r = d / 2

rvrs =
    last ++ " " ++ middle ++ " " ++ first
    where
        start = "Curry is awesome"
        first = take 5 start
        middle = take 2 $ drop 6 start
        last = drop 9 start