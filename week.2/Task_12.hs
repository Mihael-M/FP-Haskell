findOneSum :: Int -> Int -> Int -> Int
findOneSum a b n = helper (n-1) 0  
 where 
    helper 0 result  = (result+a+(findSingleSum b 0))
    helper n result = helper (n-1) (result+(findSingleSum b n ))

findSingleSum :: Int -> Int -> Int
findSingleSum b n = b*(powRec 2 n)

findSum :: Int -> Int -> Int -> Int
findSum a b n = (findOneSum a b n)+(findOneSum a b (n-1))+(findOneSum a b (n-2))

powRec :: Int -> Int -> Int
powRec _ 0 = 1
powRec x n = x * powRec x (n - 1)

main :: IO()
main = do
    print $ findSum 0 2 10 == 3578 -- 510 + 1022 + 2046
    print $ findSum 5 3 5 == 174 -- 26 + 50 + 98
    print $ findSum 16 7 11 == 25115 -- mytest