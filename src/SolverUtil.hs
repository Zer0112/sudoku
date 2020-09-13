module SolverUtil (vecSudoku, findEmpty, fsudoku, convertVectoSud, findEmptyInt) where

import qualified Data.Vector as V

import           GameField
import           Utility
-- | convert sudoku into a list of digits
-- >>> fsudoku initSudokuField2
-- [0,1,2,3,4,5,6,7,8,0,1,2,3,4,5,6,7,8,0,1,2,3,4,5,6,7,8,0,1,2,3,4,5,6,7,8,0,1,2,3,4,5,6,7,8,0,1,2,3,4,5,6,7,8,0,1,2,3,4,5,6,7,8,0,1,2,3,4,5,6,7,8,0,1,2,3,4,5,6,7,8]
-- >>> length  (fsudoku initSudokuField2) == nrOfElem^2
-- True
fsudoku :: [SudokuField] -> [Digit]
fsudoku sud = map (\(SudokuField _ _ d) -> d) sud

-- | convert sudoku to vector
-- >>> vecSudoku initSudokuField2
-- [0,1,2,3,4,5,6,7,8,0,1,2,3,4,5,6,7,8,0,1,2,3,4,5,6,7,8,0,1,2,3,4,5,6,7,8,0,1,2,3,4,5,6,7,8,0,1,2,3,4,5,6,7,8,0,1,2,3,4,5,6,7,8,0,1,2,3,4,5,6,7,8,0,1,2,3,4,5,6,7,8]
vecSudoku :: [SudokuField] ->V.Vector Digit
vecSudoku sud =V.fromList $ fsudoku sud

-- >>> findEmpty [EmptyField,One,EmptyField]
-- [0,2]
findEmpty :: [Digit] -> [Int]
findEmpty sud = findEmpty' sud 0 [] where
    findEmpty' [] _ lst = reverse lst
    findEmpty' (x:xs) ind lst
                            | x == EmptyField = findEmpty' xs (ind+1) (ind:lst)
                            | otherwise = findEmpty' xs (ind+1) lst
findEmptyInt :: [Int] -> [Int]
findEmptyInt sud = findEmpty' sud 0 [] where
    findEmpty' [] _ lst = reverse lst
    findEmpty' (x:xs) ind lst
                            | x == 0 = findEmpty' xs (ind+1) (ind:lst)
                            | otherwise = findEmpty' xs (ind+1) lst


convertVectoSud :: V.Vector Digit -> [SudokuField]
convertVectoSud sudVec = stringToSudokuHelper (V.toList sudVec) 1
