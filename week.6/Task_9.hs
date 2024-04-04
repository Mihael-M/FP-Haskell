import Data.Char

reduceStr :: String -> String
reduceStr = reverse . foldl reduce []
  where
    reduce :: String -> Char -> String
    reduce [] x = [x]
    reduce (s:stack) x
     | s /= x && toLower s == toLower x = stack
     | otherwise = x : s : stack
   
main :: IO()
main = do
    print $ reduceStr "aA" == ""
    print $ reduceStr "dabAcCaCBAcCcaDD" == "dabCBAcaDD" -- dabAcCaCBAcCcaDD -> dabAaCBAcCcaDD -> dabCBAcCcaDD -> dabCBAcaDD
    print $ reduceStr "aBbA" == "" -- mytest