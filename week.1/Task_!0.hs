
snail :: Int -> Int -> Int -> Int
snail pipeHeight upSpeed downSpeed = snailHelper 0 0
  where
    snailHelper :: Int -> Int -> Int
    snailHelper currentHeight days
      | currentHeight >= pipeHeight = days
      | currentHeight + upSpeed >= pipeHeight = days + 1
      | otherwise = snailHelper (currentHeight + upSpeed - downSpeed) (days + 1)
      

main :: IO()
main = do



    print $ snail 3 2 1 == 2
    print $ snail 10 3 1 == 5
    print $ snail 10 3 2 == 8
    print $ snail 100 20 5 == 7
    print $ snail 5 10 3 == 1
    print $ snail 10 4 2 == 4 -- my test