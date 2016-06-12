
import Test.QuickCheck
import Text.Show.Functions
import Data.List

--------------------------------------------------------------------

segments :: (a -> Bool) -> [a] -> [[a]]
segments p [] = []
segments p xs = takeWhile p xs : segments p (drop 1 (dropWhile p xs))

--------------------------------------------------------------------

prop_Take_Drop n xs =
  take n xs ++ drop n xs == (xs :: [Int])

prop_TakeWhile_DropWhile p xs =
  takeWhile p xs ++ dropWhile p xs == (xs :: [Int])

--------------------------------------------------------------------

-- `countWords s` makes a table of word occurrences in `s`
countWords :: String -> String
countWords = unlines
     . map (\(w,n) -> w++":"++show n)
     . map (\ws -> (head ws, length ws))
     . groupBy (==)
     . sort
     . words

--------------------------------------------------------------------

prop_MapMap :: (Int -> Int) -> (Int -> Int) -> [Int] -> Bool
prop_MapMap f g xs =
  map f (map g xs) == map (f . g) xs

--------------------------------------------------------------------

countWords' :: String -> IO ()
countWords' = putStr
      . unlines
      . map (\ws -> head ws ++ ":" ++ show (length ws))
      . groupBy (==)
      . sort
      . words

--------------------------------------------------------------------



revWords :: String -> String
revWords s = unwords ( map reverse ( words s))

revWords' :: String -> String
revWords' = unwords . map reverse . words
--               f                (g      )


{-
-    \ lambda
-    sms med .
-    putStr unlines för tabell
-}