import Data.List
  -- task - 1 - solution:
warmerAfter :: [Double] -> [Int]
warmerAfter [] = []
warmerAfter temperatures = map (findWarmerIndex temperatures) [0..(length temperatures - 1)]

findWarmerIndex :: [Double] -> Int -> Int
findWarmerIndex temperatures idx
 | idx == length temperatures - 1 = 0
 | otherwise = findNextWarmer 1 (drop (idx + 1) temperatures) -- Otherwise, find the next warmer day
  where 
    currentTemperature = temperatures !! idx
    findNextWarmer daysRemaining [] = 0 -- If there are no more days to check, return 0
    findNextWarmer daysRemaining (x:xs)
     | x > currentTemperature = daysRemaining -- If the temperature of the next day is warmer, return the number of days remaining
     | otherwise = findNextWarmer (daysRemaining + 1) xs -- Otherwise, continue checking the next day

  --task - 2 - solution:

calculateNewPositions :: [Int] -> String -> Int -> [Int]
calculateNewPositions positions directions t = map (\(pos, dir) -> pos + t * (if dir == 'R' then 1 else -1)) (zip positions directions)

setupRobots :: [Int] -> String -> (Int -> [Int])
setupRobots positions directions = (\t -> sort $ calculateNewPositions positions directions t)

main::IO()
main = do

  -- task - 1

  print $ warmerAfter [20,21,20,19,18,20,25,24,23,20,26] == [1,5,4,2,1,1,4,3,2,1,0]
  print $ warmerAfter [0,10,20,30] == [1,1,1,0]
  print $ warmerAfter [21,22,23] == [1,1,0]
  print $ warmerAfter [23,24,25,21,19,23,26,23] == [1,1,4,2,1,1,0,0]

  --task - 2

  print $ (setupRobots [0, 1] "LR") 3 == [-3, 4]
  print $ (setupRobots [-2, 0, 2] "RLL") 2  == [-2, 0, 0]
  print $ (setupRobots [-2, 0, 2] "RLL") 5 == [-5, -3, 3]
  print $ (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 1 == [-1,-1,0,2,5,8,9,13,14]
  print $ (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 3 == [-3,-2,0,1,7,7,10,12,15]
  print $ (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 5 == [-5,-4,-2,3,5,9,10,12,17] 