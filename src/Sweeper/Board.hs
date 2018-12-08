module Sweeper.Board where

import Data.Sequence (Seq)
import qualified Data.Sequence as S
import System.Random

data Button = Button BStatus BContents
    deriving Show

data BContents = Mine Bool | Number Int | Blank
    deriving (Show, Eq)

data BStatus = None | Flagged | Revealed | Question
    deriving (Show, Eq)

data Board = Board Size (Seq Button)
type Size = (Int, Int)
type Coord = (Int, Int)

-- Board Creation

emptyBoard :: Size -> Board
emptyBoard sz@(w,h) = Board sz $
    S.replicate (w*h) (Button None Blank)

mkBoard :: Size -> Int -> IO Board
mkBoard sz@(w,h) n = do
    let brd@(Board sz sq) = emptyBoard sz
    mines <- selectRand n (indices sz)
    return $! foldr (\c b ->
        update b c newMine) brd mines

newMine :: Button
newMine = Button None (Mine False) 

selectRand :: Int -> [a] -> IO [a]
selectRand n xs
    | n > length xs = xs
    | n <= 0        = []
    | otherwise     = do
        gen <- getStdGen
        let rs = randoms gen
            ys = zip xs rs
        return $! take n ys

indices :: Size -> [Coord]
indices (w,h) = [ (i,j) | i <- [0..w]
                        , j <- [0..h] ]

-- Board Access

(.!) :: Board -> Coord -> Button
(Board (w,_) sq) .! (x,y) = S.index sq $ w * y + x

update :: Board -> Coord -> Button -> Board
update (Board sz@(w,_) sq) (x,y) b =
    Board sz $ S.update (w * y + x) sq

inBounds :: Coord -> Board -> Bool
inBounds (x,y) (Board (w,h) _) = not $
    x < 0 || y < 0 || x >= w || y >= height

adjs :: Coord -> Board -> [Coord]
adjs (x,y) b@(Board _ sq) =
    [ix | i <- [x - 1 .. x + 1]
        , j <- [y - 1 .. y + 1]
        , let ix = (i,j)
        , not $ ix == (x,y)
        , inBounds ix b]

-- Button Handling

buttonVal :: Button -> String
buttonVal (Button Revealed (Number n)) = show n
buttonVal _                            = ""

buttonClass :: Button -> String
buttonClass b = "button" ++ buttonClass' b
    where
        buttonClass' (Button None _)     = ""
        buttonClass' (Button Flagged _)  = " button-flagged"
        buttonClass' (Button Question _) = " button-question"
        buttonClass' (Button Revealed c) = " button-revealed" ++
            case c of
                Mine True  -> " mine-tripped"
                Mine False -> " mine"
                _          -> ""

-- handle updates from buttons
-- set numbers on adjacent to mines

