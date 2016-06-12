import Data.List
import Test.QuickCheck


-- 1
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation (l1:[]) l2 = l1 `elem` l2
isPermutation (l:l1) l2 = l `elem` l2 && (isPermutation l1 l2)

isP :: Eq a => [a] -> [a] -> Bool
isP l1 l2 = and[ l `elem` l2 | l <- l1] &&  length l1 == length l2

-- 2 
sorted :: Ord a => [a] -> Bool
sorted []       = True
sorted [x]      = True
sorted (x:z:xs) = x <= z && sorted (z:xs)


srt :: Ord a => [a] -> [a]
srt xs 
       | sorted xs == False = srt ( srt' xs)
       | sorted xs = xs

srt' :: Ord a => [a] -> [a]
srt' (x:[]) = x:[]
srt' (x:y:xs) 
       | y < x = y : srt' (x:xs)
       | x <= y = x : srt' (y:xs)   


dupl' :: Eq a => [a] -> [Bool]
dupl' (x:[]) = [False]
dupl' (x:xs) = elem x xs : dupl' xs