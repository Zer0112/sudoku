module Main where

import           GameField
import           Lib
import           Utility
import           View

main :: IO ()
main = do
    exportSudoku "test2.txt" initSudokuField2
    -- readInSudoku "sudoku17.txt"
    startView
    -- print myGame
    -- where myGame = initField
