{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
module View (startView) where


import           Control.Lens.Getter
import           Control.Monad                 (replicateM, void)
import           Data.IORef                    (modifyIORef, newIORef,
                                                readIORef)
import           GameField                     (Digit (EmptyField),
                                                SudokuField (SudokuField),
                                                entry, nrBox, nrOfElem)
import           GHC.IO.Handle.Types           (Handle)
import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
import           Graphics.UI.Threepenny.JQuery
import           System.Info                   (os)
import           System.Process                (ProcessHandle, createProcess,
                                                shell)
import           Utility                       (changeDigit, fieldToChar2,
                                                initField, initSudokuField2,
                                                readInSudoku)

startView :: IO ()
startView = main

-- start a Threepenny server that listens on port 8023 (this is the default)
main :: IO ()
main = do
  -- launchAppInBrowser 8023
    startNew 1


startNew :: Int -> IO ()
startNew n = do
        sud1 <- readInSudoku n "sudoku17.txt"
        setupDefaultIO sud1

setupDefaultIO :: [SudokuField] -> IO ()
setupDefaultIO sud=
     startGUI
    (defaultConfig {jsPort = Just 8023, jsStatic = Just "static"})
    (setup sud 1)
-- build a user interface whenver a browser connects to the server

-- | convenience function that opens the 3penny UI in the default web browser
-- copied from one of the three penny examples
launchAppInBrowser ::
  Int -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
launchAppInBrowser port = case os of
  "mingw32" -> createProcess (shell $ "start " ++ url)
  "darwin"  -> createProcess (shell $ "open " ++ url)
  _         -> createProcess (shell $ "xdg-open " ++ url)
  where
    url = "http://localhost:" ++ show port

setup :: [SudokuField] -> Int->Window-> UI ()
setup sud i w = void $
  do
    store <-liftIO $ replicateM (nrOfElem ^ 2) $ newIORef 0
    count <- liftIO $ newIORef i

    -- title
    return w # set UI.title "sudoku"
    -- css
    UI.addStyleSheet w "bootstrap.css"
    UI.addStyleSheet w "mdb.min.css"

    --buttons
    buttons <- replicateM (nrOfElem * nrOfElem) UI.button

    -- initial
    let labeledbuttons = zip3 buttons sud store
    -- color the sudoku boxes
    let boxColor (SudokuField a b _)
          | even ((a - 1) `div` nrBox + ((b - 1) `div` nrBox)) = "#A9BCF5"
          | otherwise = ""
    -- color empty/playable fields
    let elmColor (SudokuField _ _ entry)
          | entry == EmptyField = "magenta"
          | otherwise = "black"
    -- hoover color text
    let hoverColorText (SudokuField _ _ entry)
          | entry == EmptyField = "green"
          | otherwise = "black"
    -- highlight changeable fields
    let hoverColorBackground field@(SudokuField _ _ entry)
          | entry == EmptyField = "#D0FA58"
          | otherwise = boxColor field
    -- style
    let styleh1 =[("color", "#A9BCF5"),
                 ("text-align", "center"),
                 ("text-shadow", "2px 2px 50px"),
                 ("box-shadow", "20px 20px 100px grey")]
        styleNext = [ ("background-color", "#A9BCF5"),
                        ("class", "btn btn-primary"),
                        ("text-align", "center"),
                        ("margin-left", "5%"),
                        ("margin-right", "5%"),
                        ("margin-top", "3%"),
                        ("border-radius", "25px"),
                        ("width", "90%")]
        styleSolve = [ ("background-color", "#A9BCF5"),
                        ("class", "btn btn-primary"),
                        ("text-align", "center"),
                        ("margin-left", "5%"),
                        ("margin-right", "5%"),
                        ("margin-top", "5px"),
                        ("border-radius", "25px"),
                        ("width", "90%")
                      ]

    -- counter of how often next was pressed
    my <- liftIO (readIORef count)
    nextB <- UI.button #. show my

    -- action of next to show the next sudoku based on IORef counter
    on UI.click nextB (\_ ->do
        liftIO $ modifyIORef count (+1)
        ind <-liftIO $ readIORef count
        x <-liftIO $readInSudoku ind "sudoku17.txt"
        setup x ind w

        -- for clearing the screen but i like it better otherwise
        -- let g = getElementsByTagName w (show ind)
        -- e <- last <$> g
        -- scrollToBottom ( e)

        )
    -- todo implement solveB
    solveB <- UI.button
    on UI.click solveB (\_ ->do
        temp <- liftIO $ readIORef count
        setup initSudokuField2 temp w
        )



    -- basic styling for sudoku buttons
    sudButtonGrid labeledbuttons elmColor boxColor
    sudButtonHoover labeledbuttons hoverColorText hoverColorBackground
    sudButtonLeave labeledbuttons elmColor boxColor
    sudClick labeledbuttons



    -- sudoku build
    getBody w
      #+ [ UI.div #+ [UI.h1 # set UI.text "Sudoku 9000 - The Game"]
             # set
               UI.style
                styleh1
               ,
           UI.div #+ createHTMLSudoku buttons # set UI.style [],
           UI.div #+ [element nextB
                    # set UI.text "next"
                    # set
                      UI.style
                      styleNext],
           UI.div
             #+ [ element solveB
                    # set UI.text "solve"
                    # set
                      UI.style
                      styleSolve
                ]
         ]

-- | Create html sudoku table
createHTMLSudoku :: [Element] -> [UI Element]
createHTMLSudoku buttons =
  [ UI.table
      #+ ( [UI.tr #+ [(UI.td # set UI.colspan nrOfElem) #+ []]]
             ++ concat [createHtmlRow x nrOfElem buttons | x <- [0 .. nrOfElem]]
         )
      # set
        UI.style
        [ ("margin-left", "auto"),
          ("margin-right", "auto"),
          ("margin-top", "10%")
        ]
  ]
  where
    createHtmlRow :: Int -> Int -> [Element] -> [UI Element]
    createHtmlRow nrRow nrEl buttonslst =
      [ UI.tr
          #+ [ UI.td #+ [element b]
               | b <- take nrEl $ drop (nrEl * nrRow) buttonslst
             ]
      ]

-- | creates the sudoku grid e.g. it colours the sudoku boxes
sudButtonGrid labeledbuttons elmColor boxColor=
    sequence_
      [ element b
          # set UI.text (fieldToChar2 t)
          # set
            UI.style
            [ ("class", "button"),
              ("color", elmColor t),
              ("width", "50px"),
              ("height", "50px"),
              ("background-color", boxColor t)
            ]
        | (b, t,_) <- labeledbuttons
      ]

-- | highlight the field on hoover if it can be changed/empty
sudButtonHoover labeledbuttons hoverColorText hoverColorBackground =
    sequence_
        [ on
            UI.hover
            b
            ( \_ ->
                element b
                    # set
                    UI.style
                    [ ("color", hoverColorText t),
                        ("background-color", hoverColorBackground t)
                    ]
            )
            | (b, t,_) <- labeledbuttons
        ]

-- | resets hoover to normal state
sudButtonLeave labeledbuttons elmColor boxColor =
    sequence_
      [ on
          UI.leave
          b
          ( \_ ->
              element b
                # set
                  UI.style
                  [("color", elmColor t), ("background-color", boxColor t)]
          )
        | (b, t,_) <- labeledbuttons
      ]

sudClick labeledbuttons = sequence_
        [ on
            UI.click
            b
            (\_ -> callChange b r)
            | (b, t, r) <- labeledbuttons , t^.entry == EmptyField
        ]


-- | the abstract sudoku game in the view
-- the values are stored in IOref and change on click to succ value mod (maxbound+emptyfield)
callChange b r = do
    liftIO $ modifyIORef r (\x ->((x+1) `mod` (nrOfElem+1) ))
    r <- liftIO $ readIORef r
    element b # set UI.text (rep r)
        where rep r | r ==0 ="-"
                    |otherwise = show r

