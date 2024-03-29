import Data.List

repeater :: String -> (Int -> String -> String)
repeater str = (\count glue -> concat (replicate (count - 1) (str ++ glue)) ++ str)


main :: IO()
main = do
    print $ (repeater "I love Haskell") 3 " " == "I love Haskell I love Haskell I love Haskell"
    print $ (repeater "Quack") 5 "!" == "Quack!Quack!Quack!Quack!Quack"
    print $ (repeater "Flip") 10 "." == "Flip.Flip.Flip.Flip.Flip.Flip.Flip.Flip.Flip.Flip" -- mytest