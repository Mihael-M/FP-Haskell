growingPlant :: Int -> Int -> Int -> Int
growingPlant upSpeed downSpeed desiredHeight = growingPlantHelper 0 0
  where
    growingPlantHelper :: Int -> Int -> Int
    growingPlantHelper height days
     | height >= desiredHeight = days
     | height + upSpeed >= desiredHeight = days + 1
     | otherwise = growingPlantHelper (height + upSpeed - downSpeed) (days + 1)


main :: IO()
main = do

    print $ growingPlant 5 2 5 == 1
    print $ growingPlant 5 2 6 == 2
    print $ growingPlant 10 9 4 == 1
    print $ growingPlant 100 10 910 == 10
    print $ growingPlant 50 10 100 == 3 -- my test