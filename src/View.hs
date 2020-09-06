module View
    ( startView
    )
where

import           Control.Monad
import           GameField
import           GHC.IO.Handle.Types         (Handle)
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
import           Safe                        (readMay)
import           System.Info                 (os)
import           System.Process              (ProcessHandle, createProcess,
                                              shell)
import           Text.Printf
import           Utility

startView :: IO ()
startView = main
-- start a Threepenny server that listens on port 8023 (this is the default)
main :: IO ()
main = do
    -- launchAppInBrowser 8023
    initStart



initStart :: IO ()
initStart =startGUI
    (defaultConfig { jsPort = Just 8023, jsStatic = Just "static" })
    setup4
-- build a user interface whenver a browser connects to the server

setup2 :: Window -> UI ()
setup2 window = void $ do
    return window # set title "Currency Converter"

    dollar <- UI.input
    euro   <- UI.input

    getBody window
        #+ [ column
                 [ grid
                     [ [string "Dollar:", element dollar]
                     , [string "Euro:", element euro]
                     ]
                 , string "Amounts update while typing."
                 ]
           ]

    euroIn   <- stepper "0" $ UI.valueChange euro
    dollarIn <- stepper "0" $ UI.valueChange dollar
    let rate = 0.7 :: Double
        withString f = maybe "-" (printf "%.2f") . fmap f . readMay

        dollarOut = withString (/ rate) <$> euroIn
        euroOut   = withString (* rate) <$> dollarIn

    element euro # sink value euroOut
    element dollar # sink value dollarOut


-- | convenience function that opens the 3penny UI in the default web browser
launchAppInBrowser:: Int -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
launchAppInBrowser port = case os of
  "mingw32" -> createProcess  (shell $ "start "    ++ url)
  "darwin"  -> createProcess  (shell $ "open "     ++ url)
  _         -> createProcess  (shell $ "xdg-open " ++ url)
  where url = "http://localhost:" ++ show port


setup4 :: Window -> UI ()
setup4 w =void $ do
    -- title
    return w # set UI.title "sudoku"

--buttons
    buttons <- replicateM (nrOfElem*nrOfElem) UI.button

-- initial actions
    let inField = initSudokuField2
    let labeledbuttons = zip buttons inField
    let boxColor (SudokuField a b _ ) | even ((a-1) `div` nrBox+ ((b-1) `div` nrBox)) = "red"
                    | otherwise = ""

    -- basic styling for sudoku buttons
    sequence_ [element b # set UI.text  (fieldToChar2 t) # set UI.style [("class","button"),("color","black"),("width","50px"),("height","50px"), ("background-color", boxColor t)] | (b,t) <- labeledbuttons]

    sequence_ [on UI.hover b  (\_ -> element b # set UI.style [("color","green")])| (b,_) <- labeledbuttons]
    sequence_ [on UI.leave b  (\_ -> element b # set UI.style [("color","black")])| (b,_) <- labeledbuttons]

    sequence_ [on UI.click b  (\_ -> element b # set UI.text (fieldToChar2 (changeDigit t)))| (b,t) <- labeledbuttons]

-- sudoku build
    getBody w
        #+
            [UI.div  #+ [UI.h1 #set UI.text "Sudoku 9000 - The Game"] # set UI.style [("color","blue"),("text-align","center")],grid
            [

            [UI.div #. "left" #+ [UI.button # set UI.text "Sudoku",UI.button # set UI.text "Sudoku",UI.button # set UI.text "Sudoku",UI.button # set UI.text "Sudoku"],

            UI.div#. "right" #+ createHTMLSudoku buttons # set UI.style [("text-align","center"),("border-style","solid"),("border-width","5px"),("margin","50px")]]]
            ]






-- | Create html sudoku table
createHTMLSudoku :: [Element] -> [UI Element]
createHTMLSudoku buttons=[UI.table #+ (    [UI.tr #+ [(UI.td # set UI.colspan 9) #+ []]]
        ++ concat[createHtmlRow x nrOfElem buttons | x<-[0..nrOfElem]])
        ]

-- | create sudoku rows
-- splitted from main function to make it more readable
createHtmlRow ::  Int -> Int -> [Element] -> [UI Element]
createHtmlRow nrRow nrEl buttonslst = [UI.tr #+ [UI.td #+ [element b] | b <- take nrEl $ drop (nrEl*nrRow) buttonslst]]
