module Sweeper where

import Sweeper.Core
import Sweeper.Board
import Sweeper.Utils
import Sweeper.Handler
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core hiding (Handler)
import qualified Control.Lens as L
import qualified Data.Sequence as S (index)

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

    -- create game
    initGame <- liftIO mkBeginner
    
    -- top panel
    flagCounter <- UI.label # set UI.text "0"
    faceButton <- UI.button
    timer <- UI.label # set UI.text "0"

    buttons <- mkButtons $ L.view (sBoard . bSize) initGame

    -- handlers
    let inputs = mapWithIx (\ix b -> setHandlers ix b) (concat buttons)

    game <- accumB initGame $ fmap head $ unions inputs

    -- classes and button text
    return faceButton # sink (UI.attr "class") (flip fmap game $ \g ->
            faceClass (L.view sState g) (L.view sClick g))

    flip mapMWithIx (concat buttons) $ \ix el ->
        return el # sink (UI.attr "class") (btnBehavior ix game buttonClass)
                  # sink UI.text (btnBehavior ix game buttonText)

    -- display / layout
    let topPanel = row 
            [ element flagCounter
            , element faceButton
            , element timer
            ]

        displayGrid = column
            [ topPanel
            , grid $ map2 element buttons
            ]

    getBody window #+ [UI.center #+ [displayGrid]]

    return ()

btnBehavior :: Int -> Behavior Sweeper -> (Button -> a) -> Behavior a
btnBehavior ix game f = flip fmap game $ \g ->
    let btns = L.view (sBoard . bButtons) g
        btn  = S.index btns ix
     in f btn

setHandlers :: Int -> Element -> Event (Sweeper -> Sweeper)
setHandlers ix el = fmap head $ unions
    [ applyHandler rightClick ix <$ UI.contextmenu el
    , unionWith (.)
        (applyHandler leftClick ix <$ UI.click el)
        (applyHandler mouseUp ix <$ UI.mouseup el)
    , applyHandler mouseDown ix <$ UI.mousedown el
    ]

applyHandler :: Handler -> Int -> Sweeper -> Sweeper
applyHandler h ix sw = h (S.index btns ix) ix sw
    where
        btns = L.view (sBoard . bButtons) sw

mkButtons :: Size -> UI [[Element]]
mkButtons (w,h) = sequence $ replicate h row
    where
        row = sequence $ replicate w btn
        btn = UI.button # set (UI.attr "oncontextmenu") "return false;" 

faceClass :: GameState -> Bool -> String
faceClass g click = "face-button " ++ faceClass g
    where
        faceClass (GameOver True)  = "win"
        faceClass (GameOver False) = "lose"
        faceClass _
            | click     = "clicking"
            | otherwise = "normal"

buttonClass :: Button -> String
buttonClass b = "button " ++ buttonClass' b
    where
        buttonClass' (Button None _)         = "button-none"
        buttonClass' (Button Flagged _)      = "button-flagged"
        buttonClass' (Button Revealed conts) =
            case conts of
                Mine True  -> "mine-tripped"
                Mine False -> "mine"
                Number i   -> "button-revealed " ++ "number-" ++ show i
                Blank      -> "button-revealed"
        buttonClass' (Button Question _)     = "button-question"

buttonText :: Button -> String
buttonText (Button Revealed (Number i)) = show i
buttonText _                            = ""