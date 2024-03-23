repeater :: String -> (Int -> String -> String)
repeater str 1 glue = str
repeater str count glue = (str ++ glue) ++ repeater str (count-1) glue

main :: IO()
main = do
    print $ (repeater "I love Haskell") 3 " " == "I love Haskell I love Haskell I love Haskell"
    print $ (repeater "Quack") 5 "!" == "Quack!Quack!Quack!Quack!Quack"
    print $ (repeater "Flip") 10 "." == "Flip.Flip.Flip.Flip.Flip.Flip.Flip.Flip.Flip.Flip" -- mytest