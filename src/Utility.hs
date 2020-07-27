module Utility
    ( initField
    )
where
import           GameField

initField :: [SudokuField]
initField =
    [ SudokuField x y [EmptyField]
    | x <- enumFrom One :: [Digit]
    , y <- enumFrom One :: [Digit]
    ]


readInSudoku = undefined

exportSudoku = undefined
