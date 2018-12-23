module Sweeper where

import Sweeper.Core
import Sweeper.Board
import Sweeper.Utils
import Sweeper.Handler
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core hiding (Handler)
import qualified Control.Lens as L
import qualified Data.Sequence as S (index)
import System.IO.Unsafe (unsafePerformIO)

runGame :: IO ()
runGame = do
    startGUI defaultConfig
        { jsPort = Just 8023
        , jsStatic = Just "./static"
        } (setup mkBeginner)

setup :: IO Sweeper -> Window -> UI ()
setup init window = do

    _ <- return window # set UI.title "Sephsweeper"
    UI.addStyleSheet window "main.css"

    -- create game / buttons
    initGame <- liftIO init

    buttons <- mkButtons $ L.view (sBoard . bSize) initGame
    faceButton <- UI.button

    beginnerButton <- UI.button # set UI.text "Beginner"
                                #. "difficulty-button beginner"
    intermediateButton <- UI.button # set UI.text "Intermediate"
                                    #. "difficulty-button intermediate"
    expertButton <- UI.button # set UI.text "Expert"
                                    #. "difficulty-button expert"

    -- handlers
    body <- getBody window
    return body # set (UI.attr "oncontextmenu") "return false;"

    gameTimer <- UI.timer
    UI.start gameTimer

    let inputs =
            (restart <$ UI.click faceButton)
            : (incTimer <$ UI.tick gameTimer)
            : (mouseUpGlobal <$ UI.mouseup body)
            : mapWithIx (\ix b -> setHandlers ix b) (concat buttons)

    game <- accumB initGame $ fmap head $ unions inputs

    -- counters
    flagCounter <- UI.label # sink UI.text (fmap (showCount . L.view sFlags) game)
                            #. "counter"
    timerLabel <- UI.label # sink UI.text (fmap (showCount . L.view sTime) game)
                           #. "counter"

    -- classes and button text
    return faceButton # sink (UI.attr "class") (flip fmap game $ \g ->
            faceClass (L.view sState g) (L.view sClick g))

    flip mapMWithIx (concat buttons) $ \ix el ->
        return el # sink (UI.attr "class") (btnBehavior ix game buttonClass)
                  # sink UI.text (btnBehavior ix game buttonText)

    -- display / layout
    diffs <- UI.div #. "diff-buttons"
                    # set children
                        [ beginnerButton
                        , intermediateButton
                        , expertButton
                        ]
    
    topPanel <- UI.div #. "top-panel"
                       # set children 
                        [ flagCounter
                        , faceButton
                        , timerLabel
                        ]

    rows <- mapM mkRow buttons        
    board <- UI.div #. "board"
                    # set children rows

    display <- UI.div #. "display"
                      # set children
                        [ diffs
                        , topPanel
                        , board
                        ]

    container <- UI.div #. "container"
                        # set children [display]

    getBody window #+ [element container]

    on UI.click beginnerButton $ const $
            changeDifficulty mkBeginner window container gameTimer

    on UI.click intermediateButton $ const $
            changeDifficulty mkIntermediate window container gameTimer

    on UI.click expertButton $ const $
            changeDifficulty mkExpert window container gameTimer

mkRow :: [Element] -> UI Element
mkRow els = UI.div # set children els
                   #. "board-row"

showCount :: Int -> String
showCount i
    | i < 10    = "00" ++ show i
    | i < 100   = "0" ++ show i
    | otherwise = show i

changeDifficulty :: IO Sweeper -> Window -> Element -> UI.Timer -> UI ()
changeDifficulty newGame window oldDisplay oldTimer = do
    delete oldDisplay
    UI.stop oldTimer
    setup newGame window

btnBehavior :: Int -> Behavior Sweeper -> (Button -> a) -> Behavior a
btnBehavior ix game f = flip fmap game $ \g ->
    let btns = L.view (sBoard . bButtons) g
        btn  = S.index btns ix
     in f btn

setHandlers :: Int -> Element -> Event (Sweeper -> Sweeper)
setHandlers ix el = fmap head $ unions
    [ unionWith (.)
        (applyHandler (ifNotOver leftClick) ix <$ UI.click el)
        (applyHandler (ifNotOver mouseUp) ix <$ UI.mouseup el)
    , applyHandler (ifNotOver rightClick) ix <$ UI.contextmenu el
    , applyHandler (ifNotOver mouseDown) ix <$ UI.mousedown el
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