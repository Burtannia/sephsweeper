module Sweeper where

import Sweeper.Core
import Sweeper.Board
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

    initGame <- mkBeginner
    

    return ()


