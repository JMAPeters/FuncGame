module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game
import System.Random

main :: IO ()
main = do 
    highScore <- getHighScore "textFiles/hs.txt"
    rng <- getStdGen
    playIO (InWindow "Game" (screenWidth, screenHeight) (10, 10)) -- Or FullScreen
                black            -- Background color
                30               -- Frames per second
                (initialState rng highScore)   -- Initial state
                view             -- View function
                input            -- Event function
                step             -- Step function

getHighScore :: FilePath -> IO Int
getHighScore path = do hs <- readFile path
                       return (read hs :: Int)