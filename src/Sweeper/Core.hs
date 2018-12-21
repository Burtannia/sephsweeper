{-# LANGUAGE TemplateHaskell #-}

module Sweeper.Core where

import Sweeper.Board
import Data.Time.Clock
import Control.Lens

data GameState = Unstarted
               | InProgress
               | GameOver Bool -- True = Won
               deriving (Show, Eq)

data Sweeper = Sweeper
    { _sBoard :: Board
    , _sFlags :: Int
    , _sStart :: UTCTime
    , _sState :: GameState
    } deriving Show

makeLenses ''Sweeper

mkGame :: Size -> Int -> IO Sweeper
mkGame sz n = do
    b <- mkBoard sz n
    t <- getCurrentTime
    return $! Sweeper b n t Unstarted

-- Standard Modes

mkBeginner :: IO Sweeper
mkBeginner = mkGame (9,9) 10

mkIntermediate :: IO Sweeper
mkIntermediate = mkGame (16,16) 40

mkExpert :: IO Sweeper
mkExpert = mkGame (16,30) 99