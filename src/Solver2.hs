

module Solver2 () where


import qualified Data.Vector as V
import           GameField
import           Solver
import           SolverUtil
import           Utility


zippedChoices :: [SudokuField] -> [(Int, [Digit])]
zippedChoices sud = map  (\ x -> (x, choic x)) emp
    where   s = fsudoku sud
            v = V.fromList s
            emp = findEmpty s
            choic i = findChoices i v

solve :: [SudokuField] -> [V.Vector Digit]
solve sud =sol ([vecSudoku sud]) (zippedChoices sud)

sol :: [V.Vector Digit] -> [(Int, [Digit])] -> [V.Vector Digit]
sol sud []             =sud
sol sud (z@(z1,z2):zs) = sol extended zs
    where extended=[s V.//[(z1,x)] | s<-sud,x<-(findChoices z1 s)]


-- >>> soltest
-- [[6,9,3,7,8,4,5,1,2,4,8,7,5,1,2,9,3,6,1,2,5,9,6,3,8,7,4,9,3,2,6,5,1,4,8,7,5,6,8,2,4,7,3,9,1,7,4,1,3,9,8,6,2,5,3,1,9,4,7,5,2,6,8,8,5,6,1,2,9,7,4,3,2,7,4,8,3,6,1,5,9]]
soltest = sol [(vecSudoku initSudokuField6)] (zippedChoices initSudokuField6)

test2 = solve initSudokuField5


updateSudoku :: [V.Vector Digit] -> Int -> [Digit] -> [V.Vector Digit] -> [V.Vector Digit]
updateSudoku [] _ [] stack = stack
updateSudoku [] _ _ _ = []
updateSudoku _ _ [] _  = []
updateSudoku (s:ss) i (c:cs) stack = updateSudoku (s:ss) i (c:cs) (fun:stack)
    where fun =s V.// [(i,c)]
