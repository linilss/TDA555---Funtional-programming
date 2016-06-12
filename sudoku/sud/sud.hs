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
isSolved rs = Nothing `notElem` (elemInSudoku rs)




-- prints a given sudoku
printSudoku :: Sudoku -> IO ()
printSudoku rs = putStr $ unlines $ splitEvery 10 (printSudoku' rs)

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
        | otherwise     = (m `notElem` ms):(isOkayBlock' ms) 
-- picks out the rows, columns and blocks of a sudoku and puts them into one list
blocks :: Sudoku -> [Block]
blocks s = rows s ++ columns s ++ block s

-- makes a sudoku into a list of blocks
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
-- picks out the columns of a sudoku
columns :: Sudoku -> [[Maybe Int]]
columns s = transpose ( rows s )

-- checks whether the blocks contains 9 values
prop_Block :: Sudoku -> Bool
prop_Block s = and ( map (\y->length y == 9 ) (block s))  && length (blocks s) == 27

-- checks whether the columns, rows and blocks are okay in the sudoku
isOkay :: Sudoku -> Bool
isOkay s = and (map isOkayBlock (blocks s))

------------------------------------------------------------------------------
-- E

type Pos = (Int,Int) 

-- finds the 1st blank cell in the sudoku
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

-- counts all the blank cells in a sudoku
numOfBlanks :: Sudoku -> Int
numOfBlanks s = length (filter (== Nothing) (elemInSudoku s) )

prop_Blank :: Sudoku -> Bool
prop_Blank (Sudoku s) = Nothing == head (take 1 (drop (snd c) (concat (take 1 (drop (fst c) s)))))
          where 
            c = blank (Sudoku s)

--  elem Nothing r = r !! snd (blank (Sudoku [r])) == Nothing 
--             | otherwise = prop_Blank (Sudoku rs)

-- infix operator to add something in a list of something in the n:th position

-- prop_something (???)
prop_opr :: (Eq a, Num a) => [a] -> (Int,a) -> Bool 
prop_opr [] (_,_) = True
prop_opr xs (x,y) = length (xs !!= (x,y)) == length xs


helpOpr :: (Eq a, Num a) => a -> a1 -> [a1] -> [a1]
helpOpr _ _ [] = []
helpOpr x y (z:xs)  | x == 0 = y:xs
      |otherwise = z:helpOpr (x-1) y xs

(!!=) :: [a] -> (Int,a) -> [a]
(!!=) xs (x,y) = helpOpr x y xs

-- adds a cell in the given position of the sudoku
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku s) (n,m) x = Sudoku (s !!= ((abs n), (update' (s!!(abs n)) (abs m) x)))

update' :: [Maybe Int] -> Int  -> Maybe Int -> [Maybe Int]
update' xs n x = xs !!= (n, x)
-- kolla vilken som är första nothing i 

prop_update :: Sudoku -> Pos -> Maybe Int -> Bool
prop_update (Sudoku rs) (x,y) z = goodPos (update (Sudoku rs) (a,b) z) (a,b) == [z]
  where a = x `mod` 9
        b = x `mod` 9

goodPos :: Sudoku -> Pos -> [Maybe Int]
goodPos (Sudoku rs) (a,b) = [c | (r,y) <- rs `zip` [0..8], (c,x) <- r `zip` [0..8], (a,b) == (y,x)]



--------------------------------------------------------------------------------------
-- F
-- solves all sudokus even though some of them takes some time
solve :: Sudoku -> Maybe Sudoku
solve s 
          | not (isSolvable s) || not (isOkay s) = Nothing
          | isSolved s = Just s
          | otherwise = solve (solve' s)


solve' :: Sudoku -> Sudoku
solve' s 
          | isOkay s && isSolved s = s 
          | otherwise = solve' (head(filter isOkay (fill [s]  (blankCoord s))))

-- generates all the blank coordinates of a sudoku
blankCoord :: Sudoku -> [Pos]
blankCoord s 
          | numOfBlanks s == 0 = []
          | otherwise = blank s : blankCoord (update s (blank s) (Just 1))

-- generates all possible sudokus given some filled cells
fill :: [Sudoku] -> [(Int,Int)] -> [Sudoku]
fill s [] = s
fill s (c:cs) = fill (filter isOkay (fill' s c)) cs

fill' :: [Sudoku] -> (Int,Int) -> [Sudoku]
fill' [] c = []
fill' (s:ss) c =  ([update s c x | x <- (filter (`notElem` (currentRowElem s c)) sudokuElem) ]) ++ (fill' ss c)

-- generates the elements of the actual row for the pos we're working on
currentRowElem :: Sudoku -> Pos -> [Maybe Int]
currentRowElem (Sudoku s) (y,x) = concat (take 1 (drop y s))

-- reads and solves a sudoku
{-
readAndSolve :: FilePath -> IO ()
readAndSolve fp = do 
    text <- readFile fp
    let s = solve (convertToSudoku text)
    if (s == Nothing) then error "(no solution)" else printSudoku (fromJust s)
-}
-- couldn't get this to work...
readAndSolve :: FilePath -> IO ()
readAndSolve fp = do 
    sud <- readSudoku fp
    if (solve sud) /= Nothing then printSudoku (fromJust ( solve ( sud ) ) ) else error "Not solvable"

-- checks if a sudoku is the solution of another
isSolutionOf :: Sudoku -> Sudoku -> Bool
s1 `isSolutionOf` s2 
              | not (isOkay s1) = False
              | s1 /= allBlankSudoku && s2 == allBlankSudoku = True
              | pos s1 == [] ||  pos s2 == [] = False
              | and [elem x (pos s1) | x <- (pos s2)] && isSolved s1 = True
              | otherwise = False

-- zips all cells together with their position to compare them in isSolutionOf
pos :: Sudoku -> [(Maybe Int, Pos)]
pos s = fltrNothingFrPos ( pos' s 0 ) 

pos' :: Sudoku -> Int -> [(Maybe Int, Pos)]
pos' (Sudoku []) n = []
pos' (Sudoku ((xs):xss)) n = zip xs [(n,x) | x <- [0..]] ++ pos' (Sudoku (xss)) (n+1) 

fltrNothingFrPos :: [(Maybe Int, Pos)] -> [(Maybe Int, Pos)]
fltrNothingFrPos [] = []
fltrNothingFrPos (x:xs) 
              | fst x == Nothing = fltrNothingFrPos xs
              | otherwise = x : fltrNothingFrPos xs

prop_SolveSound :: Sudoku -> Property
prop_SolveSound x = (solve x) /= Nothing ==> fromJust (solve x) `isSolutionOf` x


-- help functions to isSolvable that checks if there are any possibilities for any blank cell
goodCellGen :: Sudoku -> Pos -> [Maybe Int]
goodCellGen s (y,x) = filter (`notElem` (getRBC s (y,x))) sudokuElem  

getRBC :: Sudoku -> Pos -> [Maybe Int]
getRBC (Sudoku s) (y,x) = (concat (take 1 (drop y s)))++(getBlock (block (Sudoku s)) (y,x))++(concat (take 1 (drop x (columns (Sudoku s)))))

getBlock :: [Block] -> Pos -> [Maybe Int]
getBlock s (y,x) | y > 2 = getBlock (drop 3 s) ((y-3),x)
                 | y <= 2 && x >2 = getBlock (drop 1 s) (y,(x-3))
                          | otherwise = head s

isSolvable :: Sudoku -> Bool
isSolvable s = and (isSolvable' s (blankCoord s))

isSolvable' :: Sudoku -> [Pos] -> [Bool]
isSolvable' s cs = [(length (goodCellGen s c) /= 0) | c <- cs]