module Lib
    ( printWelcome
    )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


printWelcome :: IO ()
printWelcome = do
    print "Welcome to sudoku the game"
    print "Press 1 for playing the game"
    print "Press 2 for solving a sudoku"
    print "Press any other key to exit"
    arg <- getChar
    print arg
