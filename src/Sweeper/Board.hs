{-# LANGUAGE TemplateHaskell #-}

module Sweeper.Board where

import Data.Sequence (Seq)
import qualified Data.Sequence as S
import System.Random
import Data.List (sortBy)
import Control.Lens hiding (indices)

data Button = Button BStatus BContents
    deriving Show

data BContents = Mine Bool | Number Int | Blank
    deriving (Show, Eq)

data BStatus = None | Flagged | Revealed | Question
    deriving (Show, Eq)

type Size = (Int, Int)
type Coord = (Int, Int)

data Board = Board 
    { _bSize :: Size
    , _bButtons :: Seq Button
    } deriving Show

makeLenses ''Board

-- Board Creation

emptyBoard :: Size -> Board
emptyBoard sz@(w,h) = Board sz $
    S.replicate (w*h) (Button None Blank)

mkBoard :: Size -> Int -> IO Board
mkBoard sz@(w,h) n = do
    let board = emptyBoard sz
    mines <- selectRand n (indices sz)
    print mines
    return $! foldr (\c b ->
        incAdjs c $ addMine c b) board mines

addMine :: Coord -> Board -> Board
addMine c b = update c newMine b

incAdjs :: Coord -> Board -> Board
incAdjs c b =
    let as = adjs c b
     in foldr (\c b -> adjust c incButton b) b as

incButton :: Button -> Button
incButton (Button st Blank)      = Button st $ Number 1
incButton (Button st (Number i)) = Button st $ Number (i + 1)
incButton b                      = b

newMine :: Button
newMine = Button None (Mine False) 

selectRand :: Int -> [a] -> IO [a]
selectRand n xs
    | n > length xs = return xs
    | n <= 0        = return []
    | otherwise     = do
        gen <- getStdGen
        let rs = (randoms gen :: [Int])
            ys = sortBy (\(_,i) (_,j) -> compare i j) (zip xs rs)
        return $! map fst $ take n ys

indices :: Size -> [Coord]
indices (w,h) = [ (i,j) | i <- [0..w - 1]
                        , j <- [0..h - 1] ]

-- Board Access

(.!) :: Board -> Coord -> Button
(Board (w,_) sq) .! (x,y) = S.index sq $ w * y + x

update :: Coord -> Button -> Board -> Board
update (x,y) btn brd =
    let w = boardWidth brd
     in update' (w * y + x) btn brd

update' :: Int -> Button -> Board -> Board
update' ix btn brd = over bButtons (S.update ix btn) brd 

adjust :: Coord -> (Button -> Button) -> Board -> Board
adjust (x,y) f brd =
    let w = boardWidth brd
     in adjust' (w * y + x) f brd

adjust' :: Int -> (Button -> Button) -> Board -> Board
adjust' ix f brd = over bButtons (S.adjust f ix) brd

inBounds :: Coord -> Board -> Bool
inBounds (x,y) b =
    let w = boardWidth b
        h = boardHeight b
     in not $ x < 0 || y < 0 || x >= w || y >= h

adjs :: Coord -> Board -> [Coord]
adjs (x,y) b =
    [ix | i <- [x - 1 .. x + 1]
        , j <- [y - 1 .. y + 1]
        , let ix = (i,j)
        , not $ ix == (x,y)
        , inBounds ix b]

boardWidth :: Board -> Int
boardWidth b = fst $ view bSize b

boardHeight :: Board -> Int
boardHeight b = snd $ view bSize b

ix2Coord :: Int -> Size -> Coord
ix2Coord ix (w,h) =
    let (y,x) = divMod ix w
     in (x,y)