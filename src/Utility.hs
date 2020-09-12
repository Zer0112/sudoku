-- | module for debugging and utility functions
module Utility
  ( initField,
    readInSudokus,
    exportSudoku,
    fieldToChar2,
    initSudokuField2,
    initSudokuField3,
    initSudokuField4,
    initSudokuField5,
    initSudokuField6,
    stringToSudokuHelper,

    readInSudoku
  )
where

import           Control.Lens ((^.))
import           Data.Maybe   (isNothing)
import           GameField    (Digit (..), Sudoku, SudokuField (SudokuField),
                               entry, nrOfElem)


-- | creates a dummy field for testing
initField :: [SudokuField]
initField =
  [ SudokuField x y EmptyField
    | x <- [1 .. nrOfElem],
      y <- [1 .. nrOfElem]
  ]

-- | creates a dummy field for testing
initSudokuField2 :: [SudokuField]
initSudokuField2 = createSudokuField [enumFromTo EmptyField Eight | x <- [1 .. 9]]

initSudokuField3 :: [SudokuField]
initSudokuField3 =stringToSudoku "693784512487512936125963874932651487568247391741398625319475268856129743274836159"

initSudokuField4 :: [SudokuField]
initSudokuField4 =stringToSudoku "093784512487512936125963874932651487568247391741398625319475268856129743274836150"
initSudokuField5 :: [SudokuField]
initSudokuField5 =stringToSudoku "000000010400000000020000000000050407008000300001090000300400200050100000000806000"
initSudokuField6 :: [SudokuField]
initSudokuField6 =stringToSudoku "000000510087512936125000804932600480568247300041398025319470268856129700074836150"


createSudokuField :: [[Digit]] -> [SudokuField]
createSudokuField digList = concat [rowCreate nrRow dlist | (nrRow, dlist) <- zip [1 ..] digList]

rowCreate :: Int -> [Digit] -> [SudokuField]
rowCreate nrRow digList = concat [[SudokuField i nrRow d] | (d, i) <- zip digList [1 ..]]



-- inport/export functions


-- | reads in a Sudoku from a file
-- the sudoku has to be in line and empty field is 0
-- >>> readInSudokus "test2.txt"
-- [[ ,1,2,3,4,5,6,7,8, ,1,2,3,4,5,6,7,8, ,1,2,3,4,5,6,7,8, ,1,2,3,4,5,6,7,8, ,1,2,3,4,5,6,7,8, ,1,2,3,4,5,6,7,8, ,1,2,3,4,5,6,7,8, ,1,2,3,4,5,6,7,8, ,1,2,3,4,5,6,7,8]]
readInSudokus :: FilePath -> IO [Sudoku]
readInSudokus pathToFile = do
  lst <- lines <$> readFile pathToFile
  return $ map stringToSudoku lst

readInSudoku :: Int -> FilePath -> IO Sudoku
readInSudoku n pathToFile = (!! n) <$> (readInSudokus pathToFile)

-- | exports a sudoku in inline style with 0 as empty
exportSudoku :: FilePath -> Sudoku -> IO ()
exportSudoku name sudoku =
  writeFile name (sudokuToString sudoku)




-- convert string to sudoku and reverse


-- | creates a sudoku from a string
-- >>> stringToSudoku "1234"
-- [1,2,3,4]
stringToSudoku :: String -> Sudoku
stringToSudoku lst
  -- empty field if it is not a valid sudoku
  | isNothing t = initField
  | otherwise = stringToSudokuHelper2 t
  where
    t = mapM charToSud lst

-- | handeling invalid sudoku with replacing it with an empty sudoku
-- >>> stringToSudokuHelper2 (Just [One, One])
-- [1,1]
stringToSudokuHelper2 :: Maybe [Digit] -> [SudokuField]
-- error handeling for not digit
stringToSudokuHelper2 Nothing    = initField
stringToSudokuHelper2 (Just dig) = stringToSudokuHelper dig 1

-- >>> stringToSudokuHelper [One, One] 2
-- [1,1]
stringToSudokuHelper :: [Digit] -> Int -> [SudokuField]
stringToSudokuHelper lst i
  | null lst = []
  | i > nrOfElem = []
  | otherwise = rowCreate i (take nrOfElem lst) ++ stringToSudokuHelper (drop nrOfElem lst) (i + 1)

-- | creates a string from a sudoku
sudokuToString :: Sudoku -> String
sudokuToString = concatMap fieldToChar

-- | mapping from chr to sudoku entry
charToSud :: Char  -> Maybe Digit
charToSud ch = charToSudGeneral ch '0'

-- | mapping from chr to sudoku entry
charToSudGeneral :: Char ->Char -> Maybe Digit
charToSudGeneral ch emptyFormat
  | ch == emptyFormat = Just EmptyField
  | ch == '1' = Just One
  | ch == '2' = Just Two
  | ch == '3' = Just Three
  | ch == '4' = Just Four
  | ch == '5' = Just Five
  | ch == '6' = Just Six
  | ch == '7' = Just Seven
  | ch == '8' = Just Eight
  | ch == '9' = Just Nine
  | otherwise = Nothing

-- different export options
-- at the moment only intern available
-- seems everyone has their favorite format

-- | creates a String from a Sudokufield with empty = 0
fieldToChar :: SudokuField -> [Char]
fieldToChar field = digitToChar (field ^. entry)
  where
    digitToChar :: Digit -> [Char]
    digitToChar dig
      | dig == EmptyField = "0"
      | otherwise = show dig

-- | creates a String from a Sudokufield with empty = "-"
fieldToChar2 :: SudokuField -> [Char]
fieldToChar2 field = digitToChar (field ^. entry)
  where
    digitToChar :: Digit -> [Char]
    digitToChar dig
      | dig == EmptyField = "-"
      | otherwise = show dig

-- | creates a String from a Sudokufield with empty = " "
fieldToChar3 :: SudokuField -> [Char]
fieldToChar3 field = digitToChar (field ^. entry)
  where
    digitToChar :: Digit -> [Char]
    digitToChar dig
      | dig == EmptyField = " "
      | otherwise = show dig

