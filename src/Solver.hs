module Solver
    ()
where
import           GameField

solve =filter validAll.expanedChoices


expanedChoices :: Sudoku -> [Sudoku]
expanedChoices sudoku = map expandField sudoku


expandField :: SudokuField -> [SudokuField]
expandField = undefined
-- expandField sField@(SudokuField col row entry) = [SudokuField col row e | e <-entry]
