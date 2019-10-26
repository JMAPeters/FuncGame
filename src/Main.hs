module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Game" (screenWidth, screenHeight) (10, 10)) -- Or FullScreen
                black            -- Background color
                30               -- Frames per second
                initialState     -- Initial state
                view             -- View function
                input            -- Event function
                step             -- Step function