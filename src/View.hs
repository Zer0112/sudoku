module View
    ()
where
import           Control.Monad
import           Data.Functor
import           Data.IORef
import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core

import           Safe                           ( readMay )
import           Text.Printf


-- start a Threepenny server that listens on port 8023 (this is the default)
main = startGUI (defaultConfig { jsPort = Just 8023 }) setup3

-- build a user interface whenver a browser connects to the server
setup :: Window -> UI ()
setup window = do

    -- set window title
    return window # set UI.title "Sudoku"

    -- create a button element
    button  <- UI.button # set UI.text "Click me1!"
    button2 <- UI.button # set UI.text "Click me23456!"
    -- attach button to the HTML body, so that it is displayed
    getBody window #+ [element button]
    getBody window #+ [element button2]

    -- register an event handler for clicking the button
    on UI.click button $ \_ -> do
        element button # set UI.text "I have been clicked!"

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

canvasSize = 400

setup3 :: Window -> UI ()
setup3 window = do
    return window # set title "Canvas - Examples"

    UI.addStyleSheet window "semantic.css"

    canvas <-
        UI.canvas # set UI.height canvasSize # set UI.width canvasSize # set
            style
            [("border", "solid black 1px"), ("background", "#eee")]

    drawRects <- UI.button #+ [string "Add some rectangles."]
    drawText  <- UI.button #+ [string "Add text."]
    drawImage <- UI.button #+ [string "Add image."]
    drawPie   <- UI.button #+ [string "Must have pie!"]
    clear     <- UI.button #+ [string "Clear the canvas."]

    getBody window
        #+ [ column [element canvas]
           , element drawRects
           , element drawText
           , element drawImage
           , element drawPie
           , element clear
           ]

    on UI.click clear $ const $ canvas # UI.clearCanvas

    -- draw a pie chart
    on UI.click drawPie $ const $ do
        let center =
                ( fromIntegral canvasSize / 2
                , fromIntegral (canvasSize + 30) / 2
                )
            radius = 100

            drawSlice start end color = do
                canvas # set' UI.fillStyle (UI.htmlColor color)
                canvas # set' UI.strokeStyle "white"
                canvas # UI.beginPath
                canvas # UI.arc center radius start end
                canvas # UI.lineTo center
                canvas # UI.closePath
                canvas # UI.fill
                canvas # UI.stroke

            radian angle = angle * pi / 180

            normalizeAngles xs = map (\(x, y) -> (360 * x / total, y)) xs
                where total = sum $ map fst xs

            pieData = normalizeAngles
                [ (100, "#1f77b4")
                , (45 , "#ff7f0e")
                , (80 , "#2ca02c")
                , (10 , "#d62728")
                , (105, "#9467bd")
                , (20 , "#8c564b")
                ]

        UI.timestamp -- measure drawing performance for fun
        foldM
            (\start (delta, col) -> do
                let end = start + delta
                drawSlice (radian start) (radian end) col
                return end
            )
            0
            pieData
        UI.timestamp


    -- draw some rectangles
    on UI.click drawRects $ const $ do
        let rects =
                [ (20 , 130, 15, 120, "teal")
                , (345, 110, 15, 90 , "lightblue")
                , (220, 360, 95, 15 , "teal")
                ]

        forM_ rects $ \(x, y, w, h, color) -> do
            canvas # set' UI.fillStyle (UI.htmlColor color)
            canvas # UI.fillRect (x, y) w h

    -- draw some text
    on UI.click drawText $ const $ do
        return canvas
            # set UI.textFont    "30px sans-serif"
            # set UI.strokeStyle "gray"
            # set UI.fillStyle   (UI.htmlColor "black")

        canvas # UI.strokeText "is awesome" (141, 61)
        canvas # UI.fillText "is awesome" (140, 60)

    -- draw the haskell logo
    url <- UI.loadFile "image/png" "static/haskell-logo.png"
    img <- UI.img # set UI.src url

    on UI.click drawImage $ const $ do
        canvas # UI.drawImage img (60, 20)
