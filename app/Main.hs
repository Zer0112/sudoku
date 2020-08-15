module Main where

import           GameField
import           Lib
import           Utility
import           View

main :: IO ()
main = do
    startView
    print myGame
    where myGame = initField
