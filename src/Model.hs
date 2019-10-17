-- | This module contains the data types
--   which represent the state of the game
module Model where

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1

data GameState = GameState {
                   gameObjects :: GameObjects
                 , state :: State
                 , score :: Int
                 , elapsedTime :: Float
                 }

data State = Start | InGame | Pause | End

data GameObjects = GameObjects {
                   player :: Player
                 , enemies :: [Enemy]
                 , bullets :: [Bullets]
                 }

data Player = Player {
               PosX :: Int
             , PosY :: Int`
             , Speed :: Int
             , Lives :: Int
             }

data Enemy = Enemy {
               PosX :: Int
             , PosY :: Int
             , Speed :: Int
             }

data Bullet = Bullet {
               PosX :: Int
             , PosY :: Int
             , Type :: Char
}

initialState :: GameState
initialState = GameState startobjects Start 0 0

startObjects = GameObjects startPlayer [] []

startPlayer :: Player
startPlayer = Player 0 0 0 3