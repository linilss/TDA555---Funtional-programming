import Data.Char
import Data.List.Split.Internals
import System.IO
import Test.QuickCheck
import Data.Maybe

data Sudoku = Sudoku [[Maybe Int]]
	deriving (Eq, Show)
---------------------------------------------
-- a
rows :: Sudoku -> [[Maybe Int]]
rows (Sudoku rs) = rs

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

allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku [replicate 9 Nothing | n <- [1..9]]

elemInSudoku :: Sudoku -> [Maybe Int]
elemInSudoku rs = [(rows rs !! n) !! m | n <- [0..8] , m <- [0..8]]

sudokuElem :: [Maybe Int]
sudokuElem = Nothing:[Just n | n <- [1..9]]
sudokuElem' :: [Char]
sudokuElem' = ['.', '1', '2', '3', '4', '5', '6', '7', '8', '9']

isSudoku :: Sudoku -> Bool
isSudoku (Sudoku []) = False
isSudoku rs 
			| length (rows rs) == 9
			&& length (elemInSudoku rs) == 81
			&& and [n `elem` sudokuElem | n <- (elemInSudoku rs)] = True
			| otherwise 								          = False

isSolved :: Sudoku -> Bool
isSolved rs 
	| Nothing `elem` (elemInSudoku rs) = False
	| otherwise = True

printSudoku :: Sudoku -> IO ()
printSudoku rs = putStr rs'
			where
				rs' = unlines $ splitEvery 9 $ convertSudList $ elemInSudoku rs

--printSudoku rs =  elemInSudoku rsskriver ut listan med mapM_ print där varje objekt är en string


convertSudList :: [Maybe Int] -> String
convertSudList [] = []
convertSudList (r:rs) 
				| r == Nothing      = '.' : convertSudList rs
				| r `elem` sudokuElem  = r'' : convertSudList rs
			where 
				r'' = last (show r)



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
			s2 = splitOnN s

splitOnN :: String -> String
splitOnN s 
	| s == "" = ""
	| n == '\n' = splitOnN (drop 1 s)
	| otherwise = n : splitOnN (drop 1 s)
		where 
			n = head s

readSudoku :: FilePath -> IO Sudoku
readSudoku fp = readIoString fp

readIoString :: FilePath -> IO Sudoku
readIoString fp = do
	text <- readFile fp
	let rs = convertToSudoku text
	let s = splitOnN text
	if checkSud s && isSudoku rs then return  rs else error "Not a sudoku"

checkSud :: [Char] -> Bool 
checkSud s = and [s' `elem` sudokuElem' | s' <- s ]


-- --------------------------------------------------------------
-- c

cell :: Gen (Maybe Int)
cell = frequency
	[ (9, return Nothing)
	, (1, do r <- choose (1,9)
	         return (Just r))
	]

instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku rs = isSudoku rs
