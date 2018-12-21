module Sweeper where

import Sweeper.Core
import Sweeper.Board
import Sweeper.Utils
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

runGame :: IO ()
runGame = do
    startGUI defaultConfig
        { jsPort = Just 8023
        , jsStatic = Just "./static"
        } setup

setup :: Window -> UI ()
setup window = do
    _ <- return window # set UI.title "Sephsweeper"
    UI.addStyleSheet window "main.css"

    liftIO $ putStrLn "hi"

    --initGame <- mkBeginner
    
    label <- UI.label # set UI.text "Soon, or maybe spoon..."

    button <- UI.button # set UI.text "Click me"
                        # set (UI.attr "oncontextmenu") "return false;"
                        #. "button mine-tripped"

    getBody window #+ [element label, element button]

    on UI.click button $ const $ do
        element button # set UI.text "Clicked!"

    on UI.contextmenu button $ const $ do
        element button # set UI.text "Wooooh!"

    --return ()

buttonClass :: Button -> String
buttonClass b = "button " ++ buttonClass' b
    where
        buttonClass' (Button None _)         = "button-none"
        buttonClass' (Button Flagged _)      = "button-flagged"
        buttonClass' (Button Revealed conts) =
            case conts of
                Mine True  -> "mine-tripped"
                Mine False -> "mine"
                Number i   -> "button-revealed " ++ show i
                Blank      -> "button-revealed"
        buttonClass' (Button Question _)     = "button-question"

buttonText :: Button -> String
buttonText (Button Revealed (Number i)) = show i
buttonText _                            = ""