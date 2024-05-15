import Data.List
data Measuring = Temp Int Float

closestAverage :: [Measuring] -> Int
closestAverage [] = error "Empty list"
closestAverage xs = fst $ foldr (\(Temp day temp) (closestDay, closestTemp) -> if abs (temp - avg) < abs (closestTemp - avg) then (day, temp) else (closestDay, closestTemp)) (error "Empty list", 0) xs
 where 
    avg = sum [temp | Temp _ temp <- xs] / fromIntegral (length xs)
main:: IO()
main = do
    print $ closestAverage [(Temp 1 23.6), (Temp 6 24.2), (Temp 11 24.2), (Temp 16 21.2), (Temp 21 23.8), (Temp 26 26.5), (Temp 31 24.5)]
    print $ closestAverage [(Temp 2 21.5), (Temp 7 22.1), (Temp 12 23.3), (Temp 17 25.8), (Temp 22 27.4), (Temp 27 26.9), (Temp 32 23.7)] --mytest