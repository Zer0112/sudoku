module Utility
    ( initField,
    readInSudoku,
    exportSudoku
    )
where
import           Control.Lens
import           GameField
import           System.IO


initField :: [SudokuField]
initField =
    [ SudokuField x y [EmptyField]
    | x <- [1..maxBound]
    , y <- [1..maxBound]
    ]


readInSudoku pathToFile = do
    withFile pathToFile ReadMode (\h ->do
        content <-hGetLine h
        print$ stringToSudoku content
        --todo create sudoku from file
        )


exportSudoku :: FilePath -> Sudoku -> IO ()
exportSudoku name sudoku = do
    writeFile name (sudokuToString sudoku)
    -- todo creat sudoku to file

stringToSudoku :: [Char]->Sudoku
stringToSudoku lst@(x:xs)= undefined

sudokuToString :: Sudoku -> [Char]
sudokuToString lst@(x:xs)=show x ++ sudokuToString xs
    where
        lst sudoku =concat [x^.entry | x<-sudoku]

-- todo factor out just and rework

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
