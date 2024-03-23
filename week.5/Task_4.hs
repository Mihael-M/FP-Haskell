import Data.List
import Data.Char
canBeExpressed :: Int -> Bool
canBeExpressed n = any (\ x -> 4 * x + 1 == n) [1 .. n]

specialSum :: Int -> Int -> Int
specialSum start finish = sum $ filter (\ x -> canBeExpressed x && elem '6' (show x)) [start .. finish]

main :: IO()
main = do
    print $ specialSum 1 100 == 195 -- 61, 65, 69
    print $ specialSum 100 1000 == 29485 -- mytest