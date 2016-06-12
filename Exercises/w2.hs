import Data.List
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation (l:l1) l2 = l `elem` l2 && (isPermutation l1 l2)