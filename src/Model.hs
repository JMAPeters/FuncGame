-- | This module contains the data types
--   which represent the state of the game
module Model where

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1

screenWidth :: Int
screenWidth = 1280
screenHeight :: Int
screenHeight = 720

maxPlayerSpeed :: Int
maxPlayerSpeed = 5
maxBulletSpeed :: Int
maxBulletSpeed = 15
maxEnemySpeed :: Int
maxEnemySpeed = 7

enemySpawnCycle :: [Int]
enemySpawnCycle = [1,2]
enemySpawnX :: Int
enemySpawnX = 100 --(screenWidth `div` 2) + 30
spawnEnemySecs :: Float
spawnEnemySecs = 1



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
                 , bullets :: [Bullet]
                 }

data Player = Player {
               pposX :: Int
             , pposY :: Int
             , pspeed :: Int
             , plives :: Int
             }

data Enemy = Enemy {
               eposX :: Int
             , eposY :: Int
             , espeed :: Int
             , elives :: Int
             }

data Bullet = Bullet {
               bposX :: Int
             , bposY :: Int
             , bspeed :: Int 
             , btype :: BulletType
             }

data BulletType = BP | BE
instance Eq BulletType where 
  (==) BP BP = True
  (==) BE BE = True
  (==) _ _ = False

initialState :: GameState
initialState = GameState startObjects Start 0 0

startObjects = GameObjects startPlayer [] []

-- Player x y speed lives
startPlayer :: Player
startPlayer = Player ((-screenWidth `div` 2) + 30) 0 0 3