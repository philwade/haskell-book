module Arith4 where

roundTrip :: (Show a, Read b) => a -> b
roundTrip a = read (show a)

main :: IO()
main = do
    print (roundTrip 4 :: Int)
    print (roundTrip True)
    print (id 4)
    print (roundTripPF 4)

roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = read . show