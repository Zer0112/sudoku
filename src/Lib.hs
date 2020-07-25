module Lib
    ( printWelcome
    )
where
import           GameField

-- | Phrases the cl input
inputPhraser :: [Char] -> [Char]
inputPhraser arg | arg == "1" = "option 1"
                 | arg == "2" = "option 2"
                 | arg == "3" = "option 4"
                 | otherwise  = "exit"


-- | Prints welcome message in the command line
printWelcome :: IO ()
printWelcome = do
    print "Welcome to sudoku the game"
    print "Press 1 for playing the game"
    print "Press 2 for solving a sudoku"
    print "Press 3 for help"
    print "Press any other key to exit"
    arg <- getLine
    print $ inputPhraser arg

printGameM :: IO ()
printGameM = do
    print "enter col row entry to make a game move"
    print "enter q to leave"
    arg <- getLine
    print $ (inputPhraser arg)

 
