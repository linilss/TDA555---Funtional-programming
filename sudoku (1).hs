import Data.Char
import Data.List.Split.Internals
import System.IO
import Test.QuickCheck
import Data.Maybe
import Data.List

data Sudoku = Sudoku [[Maybe Int]]
	deriving (Eq, Show)
---------------------------------------------
-- a
-- splits a sudoku into the rows of itself
rows :: Sudoku -> [[Maybe Int]]
rows (Sudoku rs) = rs

-- defines an empty sudoku
emptySudoku :: Sudoku
emptySudoku = (Sudoku [])

example :: Sudoku
example =
    Sudoku
      [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
      , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
      , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
      , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
      , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
      , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
      , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
      ]

fexample :: Sudoku
fexample =
   Sudoku
     [ [Just 3, Just 6, Just 3,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
     , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
     , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
     , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
     , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
     , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
     , [Just 3,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
     , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
     , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
     ]
texample :: Sudoku
texample =
   Sudoku
     [ [Just 10, Just 6, Just 3, Just 10,Just 7, Just 1, Just 2, Nothing,Nothing]
     , [Nothing,Just 5, Nothing,Nothing,Just 10,Nothing,Just 1, Just 8, Nothing]
     , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
     , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Just 10,Just 2, Just 8]
     , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
     , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
     , [Just 3,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
     , [Nothing,Just 8, Just 3, Just 10,Nothing,Nothing,Just 10,Just 6, Nothing]
     , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
     ]


-- defines a blank sudoku, with 3x3x9 blank cells
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku [replicate 9 Nothing | n <- [1..9]]

-- picks the elements of the inner lists 
-- and concatenates the whole sudoku into a list of pure elements
elemInSudoku :: Sudoku -> [Maybe Int]
elemInSudoku rs = concat (rows rs)

-- the elements of a sudoku, either in Maybe Int- form or ./n (:: Int)-form
sudokuElem :: [Maybe Int]
sudokuElem = Nothing:[Just n | n <- [1..9]]
sudokuElem' :: [Char]
sudokuElem' = ['.', '1', '2', '3', '4', '5', '6', '7', '8', '9']

-- checks if a given sudoku is legit
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku []) = False
isSudoku rs = length (rows rs) == 9 && length (elemInSudoku rs) == 81 && and [n `elem` sudokuElem | n <- (elemInSudoku rs)] 

-- checks if a given sudoku is filled
isSolved :: Sudoku -> Bool
isSolved rs 
    | Nothing `elem` (elemInSudoku rs) = False
    | otherwise = True




-- prints a given sudoku
printSudoku :: Sudoku -> IO ()
printSudoku rs = putStr rs'
      where
         rs' = unlines $ splitEvery 10 (printSudoku' rs)

printSudoku' :: Sudoku -> [Char] 
printSudoku' (Sudoku ([]:[])) = ""
printSudoku' (Sudoku (([]):xss)) = "\n" ++ printSudoku' (Sudoku xss)
printSudoku' (Sudoku ((x:xs):xss)) = frJst x ++ printSudoku' (Sudoku (xs:xss))





-- a modified fromJust function that returns a '.' for Nothing and an 'n' from Just n
frJst :: Maybe Int -> String
frJst x | x == Nothing = "."
        | otherwise = show (fromJust x)

-- converts a string to a sudoku
convertToSudoku :: String -> Sudoku
convertToSudoku s = Sudoku( splitEvery 9 s'  )
          where s'  = stringSudConv s

stringSudConv :: String -> [Maybe Int]
stringSudConv s
     | s2 == "" = []
     | s' == '.' = Nothing : (stringSudConv (drop 1 s2))
     | s'' `elem` [1..9] = (Just s'') : (stringSudConv (drop 1 s2))
    where 
        s'  = head s
        s'' = digitToInt (head s)
        s2 = rmvN s

-- removes '\n' from a given string
rmvN :: String -> String
rmvN s = filter (/= '\n') s

-- reads a sudoku from a filepath
readSudoku :: FilePath -> IO Sudoku
readSudoku fp = readIoString fp

readIoString :: FilePath -> IO Sudoku
readIoString fp = do
    text <- readFile fp
    let rs = convertToSudoku text
    let s = rmvN text
    if checkSud s && isSudoku rs then return  rs else error "Not a sudoku"

-- checks a string-sudoku if it's legit
checkSud :: [Char] -> Bool 
checkSud s = and [s' `elem` sudokuElem' | s' <- s ]


-- --------------------------------------------------------------
-- c
-- cell generator
cell :: Gen (Maybe Int)
cell = frequency
    [ (9, return Nothing)
    , (1, do r <- choose (1,9)
    	     return (Just r))
    ]

-- defines an arbitrary sudoku
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-- property for the arbitrary sudoku
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku rs = isSudoku rs

-- ------------------------------------------------------------------------
-- d

type Block = [Maybe Int]

-- checks a block if it's okay
isOkayBlock :: Block -> Bool
isOkayBlock b = and (isOkayBlock' b)

isOkayBlock' :: Block -> [Bool]
isOkayBlock' (m:ms)
        | ms == []      = []
        | m  == Nothing = True:(isOkayBlock' ms)
        | otherwise	    = (m `notElem` ms):(isOkayBlock' ms) 

isOkayBlocks :: [Block] -> Bool 
isOkayBlocks b = and [isOkayBlock b' | b' <- b]


blocks :: Sudoku -> [Block]
blocks s = rows s ++ columns s ++ block s

block :: Sudoku -> [Block]
block (Sudoku []) = []
block (Sudoku s) = (block' (take 27 s')) ++ (block (Sudoku (drop 3 s'')))
               where 
               s' = concat (rows (Sudoku s))
               s'' = rows (Sudoku s)

block' :: [Maybe Int] -> [[Maybe Int]]
block' r = splitEvery 9 ((block'' r) ++ (block'' (drop 3 r)) ++ (block'' (drop 6 r)))

block'' :: [Maybe Int] -> [Maybe Int]
block'' k = take 3 k ++ take 3 (drop 9 k) ++ take 3 (drop 18 k)

columns :: Sudoku -> [[Maybe Int]]
columns s = transpose ( rows s )

isOkayColumn :: [[Maybe Int]] -> Bool
isOkayColumn s = and (isOkayColumn' s)

prop_Block :: Sudoku -> Bool
prop_Block s = and ( map (\y->length y == 9 ) (block s))

block2 :: Sudoku -> [Block]
block2 (Sudoku ([]:[]:[]:xss)) = block2 (Sudoku (drop 3 xss))
block2 (Sudoku s) = concat (map ( take 3 ) (take 3 s)): 
                    block2 (Sudoku (map (drop 3) (take 3 s)))
-- (concat (map ( take 3 ) (take 3 (map ( drop 3) (take 3 s))))):(concat (map ( take 3 ) (take 3 (map ( drop 6) (take 3 s))))):[]

isOkayColumn' :: [[Maybe Int]] -> [Bool]
isOkayColumn' [] = []
isOkayColumn' (xs:xss) = isOkayColumn'' xs ++ isOkayColumn' xss


isOkayColumn'' :: [Maybe Int] -> [Bool]
isOkayColumn'' (x:xs) 
            | xs == [] = []
            | x == Nothing = True:(isOkayColumn'' xs)
            |otherwise = (x `notElem` xs) : isOkayColumn'' xs

isOkay :: Sudoku -> Bool
isOkay s = isOkayBlocks (blocks s)

------------------------------------------------------------------------------
-- E

type Pos = (Int,Int) 

blank :: Sudoku -> Pos
blank s = (y,x)
      where s' = elemInSudoku s
            x = mod (blank' s') 9
            y = mod (blank'' (rows s)) 9

blank' :: [Maybe Int] -> Int
blank' (x:xs) 
    | x == Nothing =  0
    | otherwise = 1 + blank' (xs)

blank'' :: [[Maybe Int]] -> Int
blank'' (x:xs) 
    | elem Nothing x = 0
    | otherwise = 1 + blank'' xs

numOfBlanks :: Sudoku -> Int
numOfBlanks s = length (filter (== Nothing) (elemInSudoku s) )

-- prop_blank :: Sudoku -> Bool
-- prop_blank s = 

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (n, a) = (take n xs) ++ [a] ++ (drop (n+1) xs)

-- this is where a prop_ should

update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku s) (n,m) x = Sudoku (s !!= (n, (update' (s!!n) m x)))

update' :: [Maybe Int] -> Int  -> Maybe Int -> [Maybe Int]
update' xs n x = xs !!= (n, x)
-- kolla vilken som är första nothing i 




--------------------------------------------------------------------------------------
-- F

-- The solve function solves the Sudoku
-- we got stuck working with the solve function after 10+ hours of work. We'd love some advice

solve :: Sudoku -> Maybe Sudoku
solve s 
          | not (isOkay s) = Nothing
          | isOkay s && isSolved s = Just s
          | otherwise = solve (solve' s)

solve' :: Sudoku -> Sudoku
solve' s 
          | isOkay s && isSolved s = s 
          |  otherwise = solve' (head (fill9 [s] (nineFstBlanks s)))



fill :: Sudoku -> Sudoku
fill s 
          | isSolved s && isOkay s = s
          | numOfBlanks s == numOfBlanks (fillSome s (blankCoord s)) = s
          | otherwise = fill (fillSome s (blankCoord s))

fillSome :: Sudoku -> [(Int,Int)] -> Sudoku
fillSome s [] = s
fillSome s (c:cs)  
          | length x == 1 = fillSome (update s c (head x)) cs
          | otherwise = fillSome s cs
        where
            x = goodCellGen s c         

blankCoord :: Sudoku -> [(Int, Int)]
blankCoord s 
          | numOfBlanks s == 0 = []
          | otherwise = blank s : blankCoord (update s (blank s) (Just 1))

fstBlankRowCoords :: Sudoku -> [Pos]
fstBlankRowCoords s = take (length (filter (== fst (blank s)) (map fst (blankCoord s)))) (blankCoord s)

nineFstBlanks  :: Sudoku -> [Pos]
nineFstBlanks s = take 9 (blankCoord s)


fill9 :: [Sudoku] -> [(Int,Int)] -> [Sudoku]
fill9 s [] = s
fill9 s (c:cs) = fill9 (fill9' s c) cs

fill9' :: [Sudoku] -> (Int,Int) -> [Sudoku]
fill9' [] c = []
fill9' (s:ss) c =  ([update s c x | x <- (goodCellGen s c)] ) ++ (fill9' ss c)

rmv10 :: Sudoku -> [Maybe Int]
rmv10 (Sudoku []) = []
rmv10 (Sudoku (([]):xss)) = [] ++ rmv10 (Sudoku xss)
rmv10 (Sudoku ((x:xs):xss)) 
          | x == Just 10 = Nothing : rmv10 (Sudoku (xs:xss))
          | otherwise = x : rmv10 (Sudoku (xs:xss))

goodCellGen :: Sudoku -> Pos -> [Maybe Int]
goodCellGen s (y,x) = filter (`notElem` (getRBC s (y,x))) sudokuElem  

getRBC :: Sudoku -> Pos -> [Maybe Int]
getRBC (Sudoku s) (y,x) = (concat (take 1 (drop y s)))++(getBlock (block2 (Sudoku s)) (y,x))++(concat (take 1 (drop x (columns (Sudoku s)))))

getBlock :: [Block] -> Pos -> [Maybe Int]
getBlock s (y,x) | y > 2 = getBlock (drop 3 s) ((y-3),x)
                 | y <=2 && x >2 = getBlock (drop 1 s) (y,(x-3))
                          | otherwise = head s

readAndSolve :: FilePath -> IO ()
readAndSolve fp = do 
    text <- readFile fp
    let s = fill (convertToSudoku text)
    printSudoku s

{-readSudoku :: FilePath -> IO Sudoku
readSudoku fp = readIoString fp

readIoString :: FilePath -> IO Sudoku
readIoString fp = do
    text <- readFile fp
    let rs = convertToSudoku text
    let s = rmvN text
    if checkSud s && isSudoku rs then return  rs else error "Not a sudoku"
-}