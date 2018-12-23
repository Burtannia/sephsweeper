module Sweeper.Handler where

import Sweeper.Board
import Sweeper.Core
import Control.Lens
import System.IO.Unsafe (unsafePerformIO)

type Handler = Button -> Int -> Sweeper -> Sweeper

restart :: Sweeper -> Sweeper
restart s = unsafePerformIO $
    mkGame (view (sBoard . bSize) s) (view sMines s)  

ifNotOver :: Handler -> Handler
ifNotOver h = \b ix s ->
    if isGameOver s
        then s
    else h b ix $
        if isUnstarted s
            then set sState InProgress s
            else s

mouseUpGlobal :: Sweeper -> Sweeper
mouseUpGlobal s
    | isGameOver s  = s
    | otherwise     = set sClick False s

incTimer :: Sweeper -> Sweeper
incTimer s
    | isGameOver s  = s
    | isUnstarted s = s
    | otherwise     = over sTime (+1) s

isUnstarted :: Sweeper -> Bool
isUnstarted s = Unstarted == view sState s

leftClick :: Handler
leftClick (Button Revealed _) _ s = s
leftClick (Button Flagged _) _ s  = s
leftClick b ix s                  = revealButton b ix s
    where
        revealButton (Button _ (Mine _)) ix s =
            let newButton = Button Revealed (Mine True)
             in set sState (GameOver False) $
                    revealMines $ over sBoard (update' ix newButton) s
        revealButton (Button _ contents) ix s =
            let newButton = Button Revealed contents
                updatedGame = over sBoard (update' ix newButton) s
                brdSize = view (sBoard . bSize) s
             in checkWin $ case contents of
                    Number _ -> updatedGame
                    _        -> revealAdjs (ix2Coord ix brdSize) updatedGame

revealAdjs :: Coord -> Sweeper -> Sweeper
revealAdjs crd s = over sBoard (revealAdjs' crd) s
    where
        revealAdjs' crd brd =
            let btns = view bButtons brd
                crds = filter (\c -> not $ isMine $ brd .! c) (adjs crd brd)
                adjBlanks = filter (\c -> isHiddenBlank $ brd .! c) crds
                brd' = foldr (\c brd -> adjust c setRevealed brd) brd crds
             in foldr (\c b -> revealAdjs' c b) brd' adjBlanks

setRevealed :: Button -> Button
setRevealed (Button _ contents) = Button Revealed contents

isHiddenBlank :: Button -> Bool
isHiddenBlank (Button None Blank)     = True
isHiddenBlank (Button Question Blank) = True
isHiddenBlank _                       = False

checkWin :: Sweeper -> Sweeper
checkWin s = if win
    then set sState (GameOver True) s
    else s
    where
        win = foldr (\btn b -> b && cond btn) True $
                view (sBoard . bButtons) s
        cond = \b -> isMine b || isRevealed b

isMine :: Button -> Bool
isMine (Button _ (Mine _)) = True
isMine _                   = False

isRevealed :: Button -> Bool
isRevealed (Button Revealed _) = True
isRevealed _                   = False

-- |Reveals mines without tripping them
revealMines :: Sweeper -> Sweeper
revealMines = over (sBoard . bButtons) (fmap reveal)
    where
        reveal (Button _ (Mine x)) = Button Revealed (Mine x)
        reveal b                   = b

rightClick :: Handler
rightClick (Button Revealed _) _ s   = s
rightClick (Button st contents) ix s =
    over sFlags counterChange $
        over sBoard (update' ix newButton) s
    where
        numFlags  = view sFlags s
        newButton = Button newSt contents
        newSt = case st of
            Flagged  -> Question
            Question -> None
            None     -> if numFlags > 0
                            then Flagged
                            else Question
        counterChange = if st == Flagged
                            then (+ 1)
                        else if newSt == Flagged
                            then \i -> i - 1
                        else id

mouseDown :: Handler
mouseDown (Button None _) _ s     = set sClick True s
mouseDown (Button Question _) _ s = set sClick True s
mouseDown _ _ s                   = s

mouseUp :: Handler
mouseUp _ _ s = set sClick False s

isGameOver :: Sweeper -> Bool
isGameOver s = case view sState s of
    GameOver _ -> True
    _          -> False