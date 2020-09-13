
module Solver (solutions,solutionsAsSudokuField,findChoices,lookupList) where

import qualified Data.Vector as V
import           GameField   (Digit (EmptyField, One), SudokuField, nrBox,
                              nrOfElem)
import           SolverUtil  (convertVectoSud, findEmpty, fsudoku, vecSudoku)
-- utility is needed for the comment expamples
import           Utility

-- WARNING: This seem to be really slow for harder sudokus do NOT use it for the 17sudokus
-- it either is an infinite loop or i messed up the performance really bad
-- it works for the simple sudoku like initSudokuField6
-- >>> solutions initSudokuField6
-- [[6,9,3,7,8,4,5,1,2,4,8,7,5,1,2,9,3,6,1,2,5,9,6,3,8,7,4,9,3,2,6,5,1,4,8,7,5,6,8,2,4,7,3,9,1,7,4,1,3,9,8,6,2,5,3,1,9,4,7,5,2,6,8,8,5,6,1,2,9,7,4,3,2,7,4,8,3,6,1,5,9]]

solutionsAsSudokuField :: [SudokuField] -> [SudokuField]
solutionsAsSudokuField sud =convertVectoSud (head s)
                    where s = solutions sud


solutions :: [SudokuField] -> [V.Vector Digit]
solutions sud =solutionsHelper [field] [] empty
    where       field = vecSudoku sud
                empty = findEmpty $ fsudoku sud


-- utiltiy funtions / at moment not used

-- | shows if a whole sudoku is valid
-- >>> completeSudokuValid (vecSudoku initSudokuField4)
-- False
-- >>> completeSudokuValid (vecSudoku initSudokuField3)
-- True
completeSudokuValid :: V.Vector Digit -> Bool
completeSudokuValid sud =all (\x -> validEntryVec x sud) [0..nrOfElem^2-1]


-- | tests if the entry at postion i is valid
validEntryVec :: Int -> V.Vector Digit -> Bool
validEntryVec i sud = all (fun i) (lookupList i)
    where fun i x
                    | sud V.! i == EmptyField = False
                    | sud V.! x == EmptyField = True
                    | otherwise = (sud V.! x) /= (sud V.! i)

-- | tests if entry at postion i with Digit dig is valid
validEntryVecDig :: Int ->Digit -> V.Vector Digit -> Bool
validEntryVecDig i dig sud = all (fun i) (lookupList i)
    where fun i x
                    | sud V.! x == EmptyField = True
                    | otherwise = (sud V.! x) /= dig


-- help functions for solutions

-- | calculates a list of indices which are in the same box/row/col as index
-- >>> lookupList 4
-- [0,1,2,3,5,6,7,8,12,13,14,21,22,23,31,40,49,58,67,76]
lookupList :: Int -> [Int]
lookupList index =[x | x <-[0..nrOfElem^2-1] ,(fun1 x && fun2 x)|| fun3 x || fun4 x,x/=index]
        where   row a = a `div` nrOfElem
                col a = a `mod` nrOfElem
                fun1 x =(row index)`div`nrBox == (row x)`div`nrBox
                fun2 x = (col index)`div`nrBox == (col x)`div`nrBox
                fun3 x = row index == row x
                fun4 x = col index == col x


-- | finds the choices for an empty field
-- >>> findChoices 0 (vecSudoku initSudokuField4)
-- [6]
findChoices :: Int -> V.Vector Digit -> [Digit]
findChoices i sud =[ x | x <- enumFrom One , validEntryVecDig i x sud]


expandC :: V.Vector Digit -> Int -> [Digit] -> [V.Vector Digit]
expandC s i = map (\ x -> s V.// [(i, x)])



-- | help function for solution function
--  it fills up all the empty fields with their choices and returns a list with all possible sudokus
-- sudStack1 all possible sudokus up to this point
-- sudStack2 all possible sudokus after filling all possible choices for the next empty element
-- emptyList list of all empty fields - fields that need to be filled up

solutionsHelper :: [V.Vector Digit] -> [V.Vector Digit] -> [Int] -> [V.Vector Digit]
solutionsHelper st1 st2 []  = st1
solutionsHelper [] sudStack2 emptyList@(e:es)  = solutionsHelper sudStack2 []  es
solutionsHelper sudStack1@(s1:s1s) sudStack2 emptyList@(e:es)  = solutionsHelper s1s (expandChoices++sudStack2) emptyList
    where   choic = findChoices e s1
            expandChoices | null choic = expandC s1 e choic
                            | otherwise =expandC s1 e choic



