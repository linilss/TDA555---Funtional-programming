-- 1 
b = [2,3,6,1,7,9]

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (x:xs) =   
    let smallerSorted = sort [a | a <- xs, a <= x]  
        biggerSorted = sort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted

linusMaxi xs = last( sort xs)

maxi :: Integer -> Integer -> String
maxi x y = if x > y then show x else if x < y then show y else "penis"

-- 2

sumsq1 n = sum[(n^2) | n <- [1..n]]

sumsq 0 = 0
sumsq n = (n^2 + sumsq(n-1))

-- 3 

hanoi 0 = 0
hanoi n = 2 * hanoi (n-1) + 1

-- 4

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1

fib n = fib (n-1) + fib(n-2)

-- 5
 
prime n = if even n then prime(n `div` 2) else if ((n `mod` 3) == 0) then prime(n `div` 3) else n
prim  = [2,3,5,7,11,13,17,19,23,29,37]