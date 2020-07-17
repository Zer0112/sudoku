
{-# LANGUAGE TemplateHaskell #-}
module GameField
    ()
where
import           Control.Lens

-- |type for the digit in the sudoku game
data Digit = EmptyField | One | Two | Three | Four | Five | Six | Seven | Eight |Nine deriving(Show,Eq,Ord)


-- | data type to represent the whole sudoku game
data SudokuField col row = SudokuField {col::Digit,
                                        row::Digit,
                                        _entry::[Digit]}

makeLenses (''SudokuField)
