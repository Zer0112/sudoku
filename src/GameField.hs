
{-# LANGUAGE TemplateHaskell #-}
module GameField
    ( Digit(..)
    , SudokuField(..)
    , Sudoku(..)
    )
where
import           Control.Lens

-- |type for the digit in the sudoku game
data Digit = EmptyField | One | Two | Three | Four | Five | Six | Seven | Eight |Nine deriving(Eq,Ord, Enum, Bounded)
instance Show Digit where
    show EmptyField = "-"
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
-- intended for easier extention to more numbers
nrOfElem :: Int
nrOfElem = fromEnum (maxBound :: Digit)


-- | data type to represent the sudoku game as whole
newtype Sudoku =Sudoku [SudokuField]
instance Show Sudoku where
    show (Sudoku lst) = show lst

-- | data type to represent the sudoku game
data SudokuField = SudokuField {_col::Digit,
                                        _row::Digit,
                                        _entry::[Digit]}
makeLenses ''SudokuField

instance Show SudokuField where
    show field@(SudokuField _col _row _entry) =
        show (field ^. col) ++ " " ++ show (field ^. row) ++ "  " ++ show
            (field ^. entry)

-- | gives the list of sudokufield with row == dig back
rowSudokuField :: Digit -> Sudoku -> [SudokuField]
rowSudokuField dig (Sudoku lst) = [ s | s <- lst, s ^. row == dig ]

-- | gives the list of sudokufield with col == dig back
colSudokuField :: Digit -> Sudoku -> [SudokuField]
colSudokuField dig (Sudoku lst) = [ s | s <- lst, s ^. col == dig ]

-- | gives the list of sudokufield with box == dig back
boxSudokuField :: Digit -> Sudoku -> [SudokuField]
boxSudokuField dig (Sudoku lst) =
    [ s | s <- lst, s ^. col == dig, s ^. row == dig, fromEnum dig == 2 ]
-- todo write the actual box function




