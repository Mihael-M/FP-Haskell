import Data.List
sumUnevenLC :: Int -> Int -> Int
sumUnevenLC start finish = sum $ [ x | x <- [min start finish .. max start finish], odd x]

sumUnevenHOF :: Int -> Int -> Int
sumUnevenHOF start finish = sum $ filter (\ x -> odd x) [min start finish .. max start finish]
main :: IO()
main = do
    print $ sumUnevenLC 5 50 == 621
    print $ sumUnevenLC 50 1 == 625
    print $ sumUnevenLC 564 565 == 565
    print $ sumUnevenLC 220 881 == 182381 -- mytest

    print $ sumUnevenHOF 5 50 == 621
    print $ sumUnevenHOF 50 1 == 625
    print $ sumUnevenHOF 564 565 == 565
    print $ sumUnevenHOF 412 22 == 42315 -- mytest