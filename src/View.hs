module View
    ( startView
    )
where

import           Control.Monad
import           GameField

import           GHC.IO.Handle.Types         (Handle)
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

import           System.Info                 (os)
import           System.Process              (ProcessHandle, createProcess,
                                              shell)

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
    setup
-- build a user interface whenver a browser connects to the server



-- | convenience function that opens the 3penny UI in the default web browser
launchAppInBrowser:: Int -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
launchAppInBrowser port = case os of
  "mingw32" -> createProcess  (shell $ "start "    ++ url)
  "darwin"  -> createProcess  (shell $ "open "     ++ url)
  _         -> createProcess  (shell $ "xdg-open " ++ url)
  where url = "http://localhost:" ++ show port


setup :: Window -> UI ()
setup w =void $ do
    -- title
    return w # set UI.title "sudoku"

--buttons
    buttons <- replicateM (nrOfElem*nrOfElem) UI.button

-- initial
    let inField = initSudokuField2
    let labeledbuttons = zip buttons inField
    -- color the sudoku boxes
    let boxColor (SudokuField a b _ ) | even ((a-1) `div` nrBox+ ((b-1) `div` nrBox)) = "#A9BCF5"
                    | otherwise = ""
    -- color empty/playable fields
    let elmColor (SudokuField _ _ entry) | entry == EmptyField = "red"
                        | otherwise = "black"
    -- hoover color text
    let hoverColorText (SudokuField _ _ entry) | entry == EmptyField = "green"
                        | otherwise = "black"
    -- highlight changeable fields
    let hoverColorBackground field@(SudokuField _ _ entry) | entry == EmptyField = "#D0FA58"
                        | otherwise = boxColor field

    -- basic styling for sudoku buttons
    sequence_ [element b # set UI.text  (fieldToChar2 t) # set UI.style [("class","button"),("color", elmColor t),("width","50px"),("height","50px"), ("background-color", boxColor t)] | (b,t) <- labeledbuttons]

    sequence_ [on UI.hover b  (\_ -> element b # set UI.style [("color",hoverColorText t),("background-color",hoverColorBackground t)])| (b,t) <- labeledbuttons]
    sequence_ [on UI.leave b  (\_ -> element b # set UI.style [("color", elmColor t),("background-color",boxColor t)])| (b,t) <- labeledbuttons]

    sequence_ [on UI.click b  (\_ -> element b # set UI.text (fieldToChar2 (changeDigit t)))| (b,t) <- labeledbuttons]
-- todo: change it to playable
-- sudoku build
    getBody w
        #+
            [UI.div  #+ [UI.h1 #set UI.text "Sudoku 9000 - The Game"] # set UI.style [("color","blue"),("text-align","center")],

            UI.div #+ createHTMLSudoku buttons  # set UI.style [("border-style","solid"),("border-width","0px")]
            ]






-- | Create html sudoku table
createHTMLSudoku :: [Element] -> [UI Element]
createHTMLSudoku buttons=[UI.table #+ (    [UI.tr #+ [(UI.td # set UI.colspan nrOfElem) #+ []]]
        ++ concat[createHtmlRow x nrOfElem buttons | x<-[0..nrOfElem]]) # set UI.style [("margin-left","auto"),("margin-right","auto")]
        ] where
            -- | create sudoku rows
            createHtmlRow ::  Int -> Int -> [Element] -> [UI Element]
            createHtmlRow nrRow nrEl buttonslst = [UI.tr #+ [UI.td #+ [element b] | b <- take nrEl $ drop (nrEl*nrRow) buttonslst]]
