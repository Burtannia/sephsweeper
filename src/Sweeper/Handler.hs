module Sweeper.Handler where

import Sweeper.Board
import Sweeper.Core
import Control.Lens

-- |Convenience function for binding the click
-- function of a button.
-- onClick :: (Button -> a) -> Button -> Event a
-- onClick f b = f b <$ UI.click bElem

type Handler = Button -> Int -> Sweeper -> Sweeper

restart :: Sweeper -> IO Sweeper
restart s = mkGame (view (sBoard . bSize) s) (view sMines s)  

leftClick :: Handler
leftClick (Button Revealed _) _ s = s
leftClick (Button Flagged _) _ s  = s
leftClick b ix s                  = revealButton b ix s
    where
        revealButton (Button _ (Mine _)) ix s =
            let newButton = Button Revealed (Mine True)
             in revealMines $ over sBoard (update' ix newButton) s
        revealButton (Button _ contents) ix s =
            let newButton = Button Revealed contents
             in over sBoard (update' ix newButton) s

-- |Reveals mines without tripping them
revealMines :: Sweeper -> Sweeper
revealMines = over (sBoard . bButtons) (fmap reveal)
    where
        reveal (Button _ (Mine x)) = Button Revealed (Mine x)
        reveal b                   = b

rightClick :: Handler
rightClick (Button Revealed _) _ s = s
rightClick (Button st contents) ix s =
    over sBoard (update' ix newButton) s
    where
        newButton = Button newSt contents
        newSt = case st of
            Flagged  -> Question
            Question -> None
            None     -> Flagged

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

