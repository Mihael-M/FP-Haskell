

countAll :: String -> [Int]
countAll list = (countSmall list) : (countBig list) : (countSymbol list) : []

countSmall :: String -> Int
countSmall [] = 0
countSmall (x:xs) =
    if ('a' <= x && x <= 'z') then 1 + countSmall xs
    else countSmall xs

countBig :: String -> Int
countBig [] = 0
countBig (x:xs) =
    if 'A' <= x && x <= 'Z' then 1 + countBig xs
    else countBig xs

countSymbol :: String -> Int
countSymbol [] = 0
countSymbol (x:xs) =
    if not ('a' <= x && x <= 'z' || 'A' <= x && x <= 'Z') then 1 + countSymbol xs
    else countSymbol xs

br :: String -> Int
br [] = 0
br (x:xs) =
    if '0' <= x && x <= '9' then 1 + br xs
    else br xs 

lockMirror :: String -> String
lockMirror x =
    if not (lockChar x) || not(length x == 4) then "Invalid code"
    else if x == reverse x then "Unlock"
    else "Not unlock"

     
lockChar :: String -> Bool
lockChar [] = True
lockChar (x:xs) =
    if '0' <= x && x <= '9' then True && (lockChar xs)
    else False

main::IO()
main = do
    -- input <- getLine
    -- let n = input
   
    -- print(countSmall n)
    -- print(countBig n)
    -- print(countSymbol n)

    -- print(br "He11o")
    print(lockMirror "1281")


    