module Main where

import           Lib
import           GameField

main :: IO ()
main = do
    printWelcome
    print myGame
  where
    myGame = Sudoku [SudokuField One One [One], SudokuField Two One [One]]

