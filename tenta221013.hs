type Item = String

priceOf :: String -> Int
priceOf "milk" = 10
priceOf "butter" = 18
priceOf "potatoes" = 22
priceOf "chocolate" = 16

totalPrice :: (Item -> Int) -> [Item] -> Int
totalPrice f (x:xs) = f x + totalPrice f xs

-- 2
correctOCR :: [Integer] -> Bool
correctOCR xs = last (show(sum xs)) == '7'
-- correctOCR [1,2,3,4,5,2] = false

-- 3

data Store 
 = Empty 
 | Join Int Store Store

maxStore :: Store -> Int
maxStore Empty = 0 
maxStore  (Join x y z) = maximum [ n , maxStore y, maxStore z]

prop_reverse_sort :: [Int] -> Bool
prop_reverse_sort xs = sort xs == sort (reverse xs)

unixHead :: FilePath -> IO ()
unixHead fp = do
	text <- readFile fp
	textLen <- 9 + sum (map length (take 10 (splitOnN text)))
	putStrLn (take textLen text)


splitOnN :: String -> [String]
splitOnN "" = []
splitOnN xs = ln : splitOnN (drop (length ln) xs)
	where 
		ln = takeWhile (/= '\n') xs