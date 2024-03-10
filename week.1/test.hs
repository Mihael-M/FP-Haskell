isEvenIf :: Int -> String
isEvenIf x = if mod x 2 == 0 then "Yes" else "No"

isEvenGuards :: Int -> String
isEvenGuards x 
 | mod x 2 == 0 = "Yes"
 | otherwise = "No"

main :: IO()
main = do


    print $ isEvenIf 2 == "Yes"
    print $ isEvenIf 15452 == "Yes"
    print $ isEvenIf 321 == "No"

    print $ isEvenGuards 2 == "Yes"
    print $ isEvenGuards 15452 == "Yes"
    print $ isEvenGuards 321 == "No"    
   