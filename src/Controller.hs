-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gs
    | elapsedTime gs + secs > spawnEnemySecs
        = return $ gs {gameObjects = doAllSteps ((gameObjects gs) { enemies = (enemies $ gameObjects gs) ++ (enemySpawner enemySpawnCycle)})}
    | elapsedTime gs + secs > nO_SECS_BETWEEN_CYCLES
        = return $ gs {gameObjects = doAllSteps (gameObjects gs)}
    | otherwise = 
        return $ gs {elapsedTime = elapsedTime gs + secs}

doAllSteps :: GameObjects -> GameObjects
doAllSteps (GameObjects player enemies bullets) = GameObjects (movePlayer player) (enemies{- moveEnemys enemies-}) (moveBullets bullets)

-- laat het step event de speler bewegen op basis van de speed
movePlayer :: Player -> Player
movePlayer player@(Player x y speed lives)
                | checkPos player = Player x (y + speed) speed lives
                | otherwise = player

moveBullets :: [Bullet] -> [Bullet]
moveBullets bullets = filter (\bullet -> deleteBullets bullet) (map moveBullet bullets)


moveBullet :: Bullet -> Bullet 
moveBullet (Bullet x y speed btype)
                | btype == BP = Bullet (x + speed) y speed btype
                | otherwise = Bullet (x - speed) y speed btype

deleteBullets :: Bullet -> Bool
deleteBullets bullet 
                | (((bposX bullet) > -(fromIntegral screenWidth `div` 2)) && ((bposX bullet) < (fromIntegral screenWidth `div` 2))) = True
                | otherwise = False

enemySpawner :: [Int] -> [Enemy]
enemySpawner [] = []
enemySpawner (x:xs) = map (\y -> spawnEnemy {-(-(fromIntegral screenWidth `div` 2) + ((screenWidth `div` x) * y))-} 0  ) [1 .. x]

spawnEnemy :: Int -> Enemy
spawnEnemy y = Enemy enemySpawnX y maxEnemySpeed 1

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gs = return (inputKey e gs)


-- key up and down veranderen de speed wannee de key down is naar of -standaard speed of + standaard speed en wanneer de key dan los word gelaten word de speed weer op 0 gezed
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey k) state _ _) gs
    = case state of
        Down -> case k of
            --'KeyF1' -> toggle pauze
            KeyUp -> gs { gameObjects = (gameObjects gs) { player = (player $ gameObjects gs) {pspeed = maxPlayerSpeed}}}
            KeyDown -> gs { gameObjects = (gameObjects gs) { player = (player $ gameObjects gs) {pspeed = -maxPlayerSpeed}}}
            -- space down : bullet creeëren. (beweging doet ie zelf)
            KeySpace -> gs {gameObjects = (gameObjects gs) {bullets = (makePlayerBullet (player $ gameObjects gs)) : (bullets $ gameObjects gs)}}
            _ -> gs
        Up -> case k of
            KeyUp -> gs { gameObjects = (gameObjects gs) { player = (player $ gameObjects gs) {pspeed = 0}}}
            KeyDown -> gs { gameObjects = (gameObjects gs) { player = (player $ gameObjects gs) {pspeed = 0}}}
            _ -> gs
inputKey _ gs = gs

checkPos :: Player -> Bool
checkPos player = (fromIntegral((pposY player) + (pspeed player)) < (fromIntegral screenHeight/2)) && (fromIntegral((pposY player) + (pspeed player)) > -(fromIntegral screenHeight/2))

-- Bullet x y type
makePlayerBullet :: Player -> Bullet
makePlayerBullet player = Bullet (pposX player + 10 ) (pposY player) maxBulletSpeed BP

makeEnemyBullet :: Enemy -> Bullet 
makeEnemyBullet enemy = Bullet (eposX enemy - 10) (eposY enemy) maxBulletSpeed BE