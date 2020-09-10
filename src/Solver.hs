{-# LANGUAGE BangPatterns #-}

module Solver () where

import           Control.Monad
import           Control.Monad.ST
import           Data.Bit
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed.Mutable as MU
import           GameField
import           SolverUtil
import           Utility


type VecSudoku = V.Vector (V.Vector Bit)



rowSlice :: Int -> V.Vector a -> V.Vector a
rowSlice index sudoku = V.slice start nrOfElem sudoku
    where start =index - index `mod` nrOfElem

-- >>> col 12
-- [3,12,21,30,39,48,57,66,75]

-- >>> col 3
-- [3,12,21,30,39,48,57,66,75]

col :: Int -> [Int]
col index = [((index `mod` nrOfElem) + x*nrOfElem) | x <- [0.. (nrOfElem-1)]]

-- >>> findEmpty <$> fsudoku initSudokuField2
-- Just [0,9,18,27,36,45,54,63,72]
findEmpty :: V.Vector (V.Vector Bit) -> V.Vector Int
findEmpty sud = V.findIndices (\x ->x==emptyBitBasis) sud
