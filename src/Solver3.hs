{-# LANGUAGE TemplateHaskell #-}

module Solver3 () where
import           Control.Lens
import qualified Data.Vector.Unboxed as V
import           GameField
import           Solver
import           SolverUtil
import           Utility

vecSudIntEmpty :: [SudokuField] -> (V.Vector Int,[Int])
vecSudIntEmpty sud=(V.fromList $ sudD, findEmptyInt sudD)
    where sudD =map (\(SudokuField _ _ d) -> fromEnum d) sud

vecSudIn :: [SudokuField] -> V.Vector Int
vecSudIn sud= fst $ vecSudIntEmpty sud

vecSudEmpty :: [SudokuField] -> [Int]
vecSudEmpty sud= snd $ vecSudIntEmpty sud



allSolve sud [] =sud
allSolve sud emptyList@(e:es) = allSolve sudN es
    where sudN =concat $ map (\x -> fillAll x emptyList) sud

fillAll sud emptyList =new
    where new = map (\ x ->fillOne x emptyList) sud

fillOne s emptyList@(e:es) =fill s e choice []
    where choice  = findChoicesInt e s


findChoicesInt i sud = [ x | x <-[1..9]  , validEntryVecDigInt i x sud]

validEntryVecDigInt :: Int ->Int -> V.Vector Int -> Bool
validEntryVecDigInt i dig sud = all (fun i) (lookupList i)
    where fun i x
                    | sud V.! x == 0 = True
                    | otherwise = (sud V.! x) /= dig


fill s e [] stack=[]
fill s e choices@(c:cs) stack=stackNew
    where stackNew=map ( \ x ->s V.// [(e,x)] ) choices



test2= fillOne (fst test1) (snd test1)

--
test3=fillAll [fst test1] (snd test1)
test4=allSolve [[fst test1]] (snd test1)

test1 = vecSudIntEmpty initSudokuField7
