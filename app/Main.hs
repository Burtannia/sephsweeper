module Main where

import Sweeper (runGame)
import Web.Browser

main :: IO ()
main = do
    _ <- openBrowser "http://127.0.0.1:8023"
    runGame
    return ()