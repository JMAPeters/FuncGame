-- | This module contains the data types
--   which represent the state of the game
module Model where

import System.Random

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
playerSize :: Int
playerSize = 30 
enemySize :: Int
enemySize = 30
bulletSize :: Int
bulletSize = 10
randomShoot :: Int
randomShoot = 5

spawnEnemyCycle :: [Int]
spawnEnemyCycle = [1 , 3 , 5 , 3 , 6, 8, 10 , 7, 20]
enemySpawnX :: Int
enemySpawnX = (screenWidth `div` 2) + 100
enemySpawnTimer :: Int
enemySpawnTimer = 150

data GameState = GameState {
                   gameObjects :: GameObjects
                 , state :: State
                 , score :: Int
                 , elapsedTime :: Float
                 , lastEnemySpawn :: Int
                 , rng :: StdGen
                 }

data State = Start | InGame | Pause | Won | Loss
instance Eq State where 
  (==) Start Start = True
  (==) InGame InGame = True
  (==) Pause Pause = True
  (==) Won Won = True
  (==) Loss Loss = True
  (==) _ _ = False

data GameObjects = GameObjects {
                   player :: Player
                 , enemies :: ([Enemy], [Int])
                 , bullets :: [Bullet]
                 }

data Player = Player {
               pposX :: Int
             , pposY :: Int
             , psize :: Int
             , pspeed :: Int
             , plives :: Int
             }

data Enemy = Enemy {
               eposX :: Int
             , eposY :: Int
             , esize :: Int
             , espeed :: Int
             , elives :: Int
             }
instance Eq Enemy where
  Enemy posx1 posy1 size1 speed1 lives1 == Enemy posx2 posy2 size2 speed2 lives2 =
    posx1 == posx2 && posy1 == posy2 && size1 == size2 && speed1 == speed2 && lives1 == lives2 

data Bullet = Bullet {
               bposX :: Int
             , bposY :: Int
             , bsize :: Int
             , bspeed :: Int 
             , btype :: BulletType
             }
instance Eq Bullet where
  Bullet posx1 posy1 size1 speed1 type1 == Bullet posx2 posy2 size2 speed2 type2 =
    posx1 == posx2 && posy1 == posy2 && size1 == size2 && speed1 == speed2 && type1 == type2 

data BulletType = BP | BE
instance Eq BulletType where 
  (==) BP BP = True
  (==) BE BE = True
  (==) _ _ = False

initialState :: StdGen -> GameState
initialState rng = GameState startObjects Start 0 0 0 rng

startObjects = GameObjects startPlayer ([], spawnEnemyCycle) []

-- Player x y size speed lives
startPlayer :: Player
startPlayer = Player ((-screenWidth `div` 2) + 30) 0 30 0 3