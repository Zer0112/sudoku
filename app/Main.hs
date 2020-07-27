module Main where

import           Lib
import           GameField
import           Utility

main :: IO ()
main = do
    printWelcome
    print myGame
    where myGame = initField
