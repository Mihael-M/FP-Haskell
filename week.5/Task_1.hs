import Data.List
import Data.Char
isPrime :: Int -> Bool
isPrime n = [1, n] == filter (\ d -> mod n d == 0) [1 .. n]

getPrimesLC :: Int -> Int -> [Int]
getPrimesLC start finish = [ x | x <- [min start finish .. max start finish], isPrime x && elem '7' (show x) ]

getPrimesHOF :: Int -> Int -> [Int]
getPrimesHOF start finish =  filter (\ x -> isPrime x && elem '7' (show x)) [min start finish .. max start finish]

main :: IO()
main = do
    print $ getPrimesLC 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesLC 100 1 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesLC 21 555 == [37,47,67,71,73,79,97,107,127,137,157,167,173,179,197,227,257,271,277,307,317,337,347,367,373,379,397,457,467,479,487,547] -- mytest

    print $ getPrimesHOF 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesHOF 100 1 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesHOF 200 300 == [227,257,271,277] -- mytest