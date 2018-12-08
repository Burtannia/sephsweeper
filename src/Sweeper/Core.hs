{-# LANGUAGE TemplateHaskell #-}

module Sweeper.Core where

import Sweeper.Board
import Data.Time.Clock
import Control.Lens

data Sweeper = Sweeper
    { _sBoard :: Board
    , _sFlags :: Int
    , _sStart :: UTCTime
    , _sState :: GameState
    }

makeLenses ''Sweeper

data GameState = Unstarted
               | InProgress
               | GameOver Bool -- True = Won

mkGame :: Size -> Int -> IO Sweeper
mkGame sz n = Sweeper (mkBoard sz n) n Unstarted

-- Standard Modes

mkBeginner :: IO Sweeper
mkBeginner = mkGame (9,9) 10

mkIntermediate :: IO Sweeper
mkIntermediate = mkGame (16,16) 40

mkExpert :: IO Sweeper
mkExpert = mkGame (16,30) 99