-- Assignment 1 
-- It takes (k+1) computing steps depending on what k is.

-- Assignment 2

power1 n k = product [n | _ <- [1 .. k]]

-- Assignment 3

power2 :: Integer -> Integer -> Integer
power2 n 0 = 1
power2 n k = if even k then (n * n) ^ (k `div` 2) else n * (n ^ (k - 1))