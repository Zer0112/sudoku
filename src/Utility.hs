module Utility
    ( initField,
    readInSudoku,
    exportSudoku,
    fieldToChar2,
    initSudokuField2,
    changeDigit,
    )
where
import           Control.Lens
import           GameField
import           System.IO

-- |creates a dummy field for testing
initField :: [SudokuField]
initField =
    [ SudokuField x y Two
    | x <- [1..maxBound]
    , y <- [1..maxBound]
    ]

-- |creates a dummy field for testing
initSudokuField2 :: [SudokuField]
initSudokuField2 = createSudokuField [enumFromTo EmptyField Eight | x<-[1..9]]

-- >>> test
-- Prelude.undefined
-- CallStack (from HasCallStack):
--   error, called at libraries\base\GHC\Err.hs:78:14 in base:GHC.Err
--   undefined, called at d:\Documents\Code\haskell\sudoku\src\Utility.hs:55:27 in fake_uid:Utility
test :: IO ()
test =print $ stringToSudoku ['1', '2', '3', '4', '5']
test2 =print $ sudokuToString initSudokuField2
-- >>> test2
createSudokuField :: [[Digit]] -> [SudokuField]
createSudokuField digList = concat [rowCreate nrRow dlist|(nrRow,dlist) <-zip [1..] digList]

rowCreate :: Int-> [Digit] -> [SudokuField]
rowCreate nrRow digList =concat[[SudokuField i nrRow d] | (d,i)<- zip digList [1..]]

-- | reads in a Sudoku from a file
-- the sudoku has to be in line and empty field is 0
readInSudoku :: FilePath -> IO [SudokuField]
readInSudoku pathToFile = do
    withFile pathToFile ReadMode (\h ->do
        content <-hGetLine h
        print$ stringToSudoku content
        return initSudokuField2
        --todo create sudoku from file
        )

-- | exports a sudoku in inline style with 0 as empty
exportSudoku :: FilePath -> Sudoku -> IO ()
exportSudoku name sudoku = do
    writeFile name (sudokuToString sudoku)

-- | creates a sudoku from a string
stringToSudoku :: [Char]->Maybe Sudoku
stringToSudoku lst@(x:xs)=undefined
    where t= fmap charToSud lst

-- | creates a string from a sudoku
sudokuToString :: Sudoku -> [Char]
sudokuToString lst=concat $ fmap fieldToChar (lst)

-- | mapping from chr to sudoku entry
charToSud :: Char -> Maybe Digit
charToSud ch
    | ch=='0' =Just EmptyField
    | ch=='1' =Just One
    | ch=='2' =Just Two
    | ch=='3' =Just Three
    | ch=='4' =Just Four
    | ch=='5' =Just Five
    | ch=='6' =Just Six
    | ch=='7' =Just Seven
    | ch=='8' =Just Eight
    | ch=='9' =Just Nine
    | otherwise = Nothing
-- todo maybe rewrite it with the use of enum

--todo in general deal with incorrect sudoku formats
-- | creates a String from a Sudokufield with empty = 0
fieldToChar :: SudokuField -> [Char]
fieldToChar field = digitToChar (field^.entry)
    where
    digitToChar :: Digit -> [Char]
    digitToChar dig
        | dig==EmptyField ="0"
        | otherwise =show dig

-- |creates a String from a Sudokufield with empty = " "
fieldToChar2 :: SudokuField -> [Char]
fieldToChar2 field = digitToChar (field^.entry)
    where
    digitToChar :: Digit -> [Char]
    digitToChar dig
        | dig==EmptyField ="-"
        | otherwise =show dig

changeDigit :: SudokuField->SudokuField
changeDigit (SudokuField row col ent) =SudokuField row col (incDig ent)
    where
        incDig lst=succ1 lst
        succ1 dig
                | dig==(maxBound :: Digit) = EmptyField
                |otherwise =succ dig



