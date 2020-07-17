
{-# LANGUAGE TemplateHaskell #-}
module GameField
    ( Digit(..)
    , SudokuField(..)
    , Sudoku(..)
    )
where
import           Control.Lens

-- |type for the digit in the sudoku game
data Digit = EmptyField | One | Two | Three | Four | Five | Six | Seven | Eight |Nine deriving(Eq,Ord, Enum)
instance Show Digit where
    show EmptyField = show () ++ "-"
    show One        = show (1)
    show Two        = show (2)
    show Three      = show (3)
    show Four       = show (4)
    show Five       = show (5)
    show Six        = show (6)
    show Seven      = show (7)
    show Eight      = show (8)
    show Nine       = show (9)


-- | data type to represent the sudoku game as whole
newtype Sudoku =Sudoku [SudokuField]
instance Show Sudoku where
    show (Sudoku lst) = show lst

-- | data type to represent the sudoku game
data SudokuField = SudokuField {col::Digit,
                                        row::Digit,
                                        _entry::[Digit]}
makeLenses (''SudokuField)

instance Show SudokuField where
    show field@(SudokuField col row _entry) =
        " | "
            ++ (show col)
            ++ " "
            ++ (show row)
            ++ "  "
            ++ show (field ^. entry)
            ++ " | "



