{-# LANGUAGE TemplateHaskell #-}

module Sweeper.Core where

import Sweeper.Board
import Control.Lens

data GameState = Unstarted
               | InProgress
               | GameOver Bool -- True = Won
               deriving (Show, Eq)

data Sweeper = Sweeper
    { _sBoard :: Board
    , _sMines :: Int
    , _sFlags :: Int
    , _sTime  :: Int
    , _sState :: GameState
    , _sClick :: Bool
    } deriving Show

makeLenses ''Sweeper

mkGame :: Size -> Int -> IO Sweeper
mkGame sz n = do
    b <- mkBoard sz n
    return $! Sweeper b n n 0 Unstarted False

-- Standard Modes

data Difficulty = Beginner | Intermediate | Expert
    deriving (Show, Eq)

mkBeginner :: IO Sweeper
mkBeginner = mkGame (9,9) 10

mkIntermediate :: IO Sweeper
mkIntermediate = mkGame (16,16) 40

mkExpert :: IO Sweeper
mkExpert = mkGame (16,30) 99