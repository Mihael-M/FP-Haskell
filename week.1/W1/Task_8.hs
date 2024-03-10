canCarry :: Int -> Double -> Double -> String
canCarry c k w 
 | c < 0 = "The number of products must be non-negative"
 | k < 0 = "The kilograms must be non-negative"
 | w < 0 = "The item weigh must be non-negative"
 | fromIntegral c * w <= k = "Yes"
 | otherwise = "No"


main :: IO()
main = do


    print $ canCarry 5 15 3 == "Yes"
    print $ canCarry 1 5 4 == "Yes"
    print $ canCarry 13 25 2 == "No"
    print $ canCarry 24 104.44 21.12 == "No"
    print $ canCarry 51 34.75 19.852 == "No"
    print $ canCarry 42 95.11 0.51 == "Yes"
    print $ canCarry 10 100 10 == "Yes" -- my test


