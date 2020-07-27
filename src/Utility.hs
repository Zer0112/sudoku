module Utility
    ()
where
import           GameField

initField :: [SudokuField]
initField =
    [ SudokuField x y [EmptyField]
    | x <- enumFromThen One Nine :: [Digit]
    , y <- enumFromThen One Nine :: [Digit]
    ]


readInSudoku = undefined

exportSudoku = undefined
