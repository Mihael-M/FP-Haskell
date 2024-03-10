






finalGrade :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
finalGrade d1 d2 d3 kz1 kz2 kt1 kt2 iz it = finalGradeSolve (tK (avg3 d1 d2 d3) (avg2 kz1 kz2) (avg2 kt1 kt2)) (examGrades it (avg2 kt1 kt2) kt1 kt2) (examGrades iz (avg2 kz1 kz2) kz1 kz2)
    where 
        avg3 gr1 gr2 gr3 = (gr1 + gr2 + gr3) / 3
        avg2 gr1 gr2 = (gr1 + gr2) / 2
        tK d kz kt = d / 4 + 3 * kt / 8 + 3 * kz / 8
        examGrades grade kzgrade kzres1 kzres2
         | kzgrade >= 4.5 && kzgrade > grade && kzres1 >= 4 && kzres2 >= 4 = kzgrade
         | otherwise = grade
        finalGradeSolve d kt kz
         | d / 2 + kt / 4 + kz / 4 < 2 = 2
         | otherwise = fromIntegral (round (100 * result)) / 100
            where
             result = d / 2 + kt / 4 + kz / 4











main :: IO()
main = do
    print $ finalGrade 3 4 4 4.25 4.50 3.75 4.25 5 4.25 == 4.34
    print $ finalGrade 6 6 6 4.50 5 4.50 4.75 5 4.75 == 4.95
    print $ finalGrade 6 0 4 6 6 5 4.75 6 4.75 == 5.17
    print $ finalGrade 4.25 0 3 2 0 0 0 0 0 == 2
    print $ finalGrade 5.50 6 6 6 5.50 5.25 4 5.50 4 == 5.27
    print $ finalGrade 6 6 6 5.50 5.50 4 5 5.50 5 == 5.25
    print $ finalGrade 6 6 6 5.25 6 4 4 5.63 3.50 == 4.84
    print $ finalGrade 6 2 3 5 4 4 5 4.50 4.50 == 4.4 --my test

