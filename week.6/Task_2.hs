import Data.List
type Cylinder = (Double, Double)

roundTwoDig :: Double -> Double
roundTwoDig n = (fromIntegral $ round $ n * 100) / 100

getVolumes :: [Cylinder] -> [Double]
getVolumes xs = map (roundTwoDig) $ map (\(r,h) -> r ^ 2 * pi * h) xs

main :: IO()
main = do
    print $ getVolumes [(5, 10), (5, 2), (2, 10), (2, 5)] == [785.4, 157.08, 125.66, 62.83]
    print $ getVolumes [(5, 10), (5, 6), (4, 10), (8, 5)] == [785.4,471.24,502.65,1005.31] -- mytest