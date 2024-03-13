calcSeriesSum :: Double -> Double -> Double
calcSeriesSum x n = helper (n+1) 3 0 (-2) 1
 where 
    helper 0 devisor result el power = result
    helper n devisor result el power = helper (n-1) (devisor+2) (result+el) ((el*(-2)*(x^power))/devisor) (power+1)
main :: IO()
main = do
    -- you may get slightly different results eg. -1.047619047619100 on test 4 <- not a problem
    print $ calcSeriesSum 1 0 == -2.0 -- x = 1, n = 0
    print $ calcSeriesSum 1 1 == -0.6666666666666667
    print $ calcSeriesSum 1 2 == -1.2000000000000002
    print $ calcSeriesSum 1 3 == -1.047619047619048
    print $ calcSeriesSum 1 4 == -1.0814814814814817
    print $ calcSeriesSum 1 5 == -1.0753246753246755
    print $ calcSeriesSum 1 6 == -1.0762718762718764
    print $ calcSeriesSum 1 7 == -1.0761455828122495 -- mytest
