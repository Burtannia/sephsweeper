module Sweeper.Board where

import Data.Sequence (Seq)
import qualified Data.Sequence as S
import System.Random
import Data.List (sortBy)

data Button = Button BStatus BContents
    deriving Show

data BContents = Mine Bool | Number Int | Blank
    deriving (Show, Eq)

data BStatus = None | Flagged | Revealed | Question
    deriving (Show, Eq)

data Board = Board Size (Seq Button)
    deriving Show

type Size = (Int, Int)
type Coord = (Int, Int)

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
addMine c b = update b c newMine

incAdjs :: Coord -> Board -> Board
incAdjs c b =
    let as = adjs c b
     in foldr (\c b -> adjust b c incButton) b as

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

update :: Board -> Coord -> Button -> Board
update (Board sz@(w,_) sq) (x,y) b =
    Board sz $ S.update (w * y + x) b sq

adjust :: Board -> Coord -> (Button -> Button) -> Board
adjust (Board sz@(w,_) sq) (x,y) f =
    Board sz $ S.adjust f (w * y + x) sq

inBounds :: Coord -> Board -> Bool
inBounds (x,y) (Board (w,h) _) = not $
    x < 0 || y < 0 || x >= w || y >= h

adjs :: Coord -> Board -> [Coord]
adjs (x,y) b@(Board _ sq) =
    [ix | i <- [x - 1 .. x + 1]
        , j <- [y - 1 .. y + 1]
        , let ix = (i,j)
        , not $ ix == (x,y)
        , inBounds ix b]

-- Debugging

printAscii :: Board -> IO ()
printAscii (Board (_,0) _)  = return ()
printAscii (Board (w,h) sq) = do
    putStrLn $ foldr1 (++) $ S.intersperse " "
        $ fmap asciiButton $ S.take w sq
    printAscii $ Board (w, h-1) (S.drop w sq)

asciiButton :: Button -> String
asciiButton (Button _ (Number i)) = show i
asciiButton (Button _ (Mine _))   = "X"
asciiButton _                     = "-"