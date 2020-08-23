
{-# LANGUAGE TemplateHaskell #-}
module GameField
    ( Digit(..)
    , SudokuField(..)
    , Sudoku(..)
    ,validAll,
    entry
    )
where
import           Control.Lens

-- |type for the digit in the sudoku game
data Digit = EmptyField | One | Two | Three | Four | Five | Six | Seven | Eight |Nine deriving(Eq,Ord, Enum, Bounded)
instance Show Digit where
    show EmptyField = show 0
    show One        = show 1
    show Two        = show 2
    show Three      = show 3
    show Four       = show 4
    show Five       = show 5
    show Six        = show 6
    show Seven      = show 7
    show Eight      = show 8
    show Nine       = show 9


-- | returns for how many numbers the sudoku is made - 9 for normal sudoku
-- intended for easier extention to more numbers if i want to play around with it later
-- >>> nrOfElem
-- 9
nrOfElem :: Int
nrOfElem = fromEnum (maxBound :: Digit)
-- | nr of boxes in one row/col
nrBox :: Int
nrBox = 3

-- | data type to represent the sudoku game as whole
type Sudoku = [SudokuField]


-- | data type to represent the sudoku game
data SudokuField = SudokuField {_col::Int,
                                        _row::Int,
                                        _entry::[Digit]}
makeLenses ''SudokuField

instance Show SudokuField where
    show field =
        show (field ^. col) ++ " " ++ show (field ^. row) ++ "  " ++ show
            (field ^. entry)


-- | gives True if both entries are in the same row
-- >>> rowFilter (SudokuField One One [One]) (SudokuField One One [One])
-- True

rowFilter :: SudokuField -> SudokuField -> Bool
rowFilter sudField1 sudField2 = sudField1 ^. row == sudField2 ^. row

-- | give True if both entries are in the same col
colFilter :: SudokuField -> SudokuField -> Bool
colFilter sudF1 sudF2 = sudF1 ^. col == sudF2 ^. col

-- | gives True if both entries are in the same box
boxFilter :: SudokuField -> SudokuField -> Bool
boxFilter sudF1 sudF2 =
    fromEnum (sudF1 ^. col)
        `div` nrBox
        ==    fromEnum (sudF2 ^. col)
        `div` nrBox
        &&    fromEnum (sudF1 ^. row)
        `div` nrBox
        ==    fromEnum (sudF2 ^. row)
        `div` nrBox

-- | filter sudokufield to be in same row and col and box but not the same
allFilter :: SudokuField -> SudokuField -> Bool
allFilter =
    (\sud x -> ((boxFilter sud x) || (colFilter sud x) || boxFilter sud x)
        && not ((sud ^. col == x ^. col) && (sud ^. col == x ^. col))
    )

-- | basic condition for a valid entry check
validOne :: SudokuField -> SudokuField -> Bool
validOne f1 f2 = f1 ^. entry /= f2 ^. entry

-- | List of all to check entries for one entry
listOfTestValid :: SudokuField -> [SudokuField] -> [SudokuField]
listOfTestValid field sudo = filter (allFilter field) sudo

-- | checks if entry is valid
validEntry :: SudokuField -> [SudokuField] -> Bool
validEntry field sudoku = and filterEntry
    where filterEntry = (map (\x -> (validOne field x)) sudoku)

-- | checks the whole sudoku
validAll :: Sudoku -> Bool
validAll sudoku = and $ map (\x -> validEntry x sudoku) sudoku

