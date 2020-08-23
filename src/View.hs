module View
    ( startView
    )
where
import           Control.Monad
import           Data.Functor
import           Data.IORef
import           GHC.IO.Handle.Types         (Handle)
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
import           Safe                        (readMay)
import           System.Info                 (os)
import           System.Process              (ProcessHandle, createProcess,
                                              shell)
import           Text.Printf
startView = main
-- start a Threepenny server that listens on port 8023 (this is the default)
main = startGUI
    (defaultConfig { jsPort = Just 8023, jsStatic = Just "static" })
    setup4

-- build a user interface whenver a browser connects to the server
setup :: Window -> UI ()
setup window = do

    -- set window titl
    return window # set UI.title "Sudoku"

    -- create a button element
    button  <- UI.button #set UI.style [("color","green"),("background","black")] # set UI.text "Click me1!"
    button2 <- UI.button # set UI.text "Click me23456!"
    -- attach button to the HTML body, so that it is displayed
    getBody window #+ [element button]
    getBody window #+ [element button2]

    -- register an event handler for clicking the button
    on UI.hover button $ \_ -> do
        element button # set UI.style [("color","red"),("background","blue")]
    on UI.leave button $ \_ -> do
        element button # set UI.style [("color","green"),("background","black")]

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



-- | launch application in default web browser
-- up :: IO ()
-- up = do
--   let port = 8023
--   launchAppInBrowser port
--   start port

-- | convenience function that opens the 3penny UI in the default web browser
launchAppInBrowser:: Int -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
launchAppInBrowser port = case os of
  "mingw32" -> createProcess  (shell $ "start "    ++ url)
  "darwin"  -> createProcess  (shell $ "open "     ++ url)
  _         -> createProcess  (shell $ "xdg-open " ++ url)
  where url = "http://localhost:" ++ show port

setup4 w =void $ do
    buttons <- replicateM 81 UI.button
    let labeledbuttons = zip buttons [show x | x<- [1..81]]
    sequence_ [element b # set UI.text t # set UI.style [("color","green"),("background","black")] | (b,t) <- labeledbuttons]
    -- sequence_ [on UI.hover b    (\_ -> callback b t) | (b,t) <- labeledbuttons]
    getBody w #+ [UI.table #+ (    [UI.tr #+ [(UI.td # set UI.colspan 9) #+ []]]
        ++ [UI.tr #+ [UI.td #+ [element b] | b <- take 9 buttons]]
        ++ [UI.tr #+ [UI.td #+ [element b] | b <- take 9 $ drop 9 buttons]]
        ++ [UI.tr #+ [UI.td #+ [element b] | b <- take 9 $ drop 18 buttons]]
        ++ [UI.tr #+ [UI.td #+ [element b] | b <- take 9 $ drop 27 buttons]]
        ++ [UI.tr #+ [UI.td #+ [element b] | b <- take 9 $ drop 36 buttons]]
        ++ [UI.tr #+ [UI.td #+ [element b] | b <- take 9 $ drop 45 buttons]]
        ++ [UI.tr #+ [UI.td #+ [element b] | b <- take 9 $ drop 54 buttons]]
        ++ [UI.tr #+ [UI.td #+ [element b] | b <- take 9 $ drop 63 buttons]]
        ++ [UI.tr #+ [UI.td #+ [element b] | b <- take 9 $ drop 72 buttons]])
        ]
