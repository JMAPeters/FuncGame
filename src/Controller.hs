-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gs@(GameState gameObjects state score elapsedTime lastEnemySpawn rng)
    | state /= InGame
        = return $ gs
    | elapsedTime + secs > nO_SECS_BETWEEN_CYCLES
        = return $ checkWinLoss (doAllSteps gs (lastEnemySpawn + 1) )
    | otherwise = 
        return $ gs {elapsedTime = elapsedTime + secs}

doAllSteps :: GameState -> Int -> GameState
doAllSteps gs n
        | n >= enemySpawnTimer = unTuple gs 0 (doMainSteps ((gameObjects $ gs) { enemies = enemySpawner(enemies $ gameObjects $ gs)}) (rng $ gs))
        | otherwise = unTuple gs n (doMainSteps (gameObjects $ gs) (rng $ gs))

unTuple :: GameState -> Int -> (GameObjects, Int) -> GameState
unTuple (GameState gameObjects state score elapsedTime lastEnemySpawn rng) n (go, points) = (GameState go state (score+points) elapsedTime n rng)
-- return tuple van GameObject en Score 
doMainSteps :: GameObjects -> StdGen -> (GameObjects, Int)
doMainSteps (GameObjects player (enemies, x) bullets) rng = doCollision (GameObjects (movePlayer player) ((moveEnemies enemies), x) (moveBullets bullets{-(spawnRandomBullets bullets enemies rng)-}))

-- laat het step event de speler bewegen op basis van de speed
movePlayer :: Player -> Player
movePlayer player@(Player x y size speed lives)
                | checkPos player = Player x (y + speed) size speed lives
                | otherwise = player

moveBullets :: [Bullet] -> [Bullet]
moveBullets bullets = filter (\bullet -> deleteBullets bullet) (map moveBullet bullets)

moveBullet :: Bullet -> Bullet 
moveBullet (Bullet x y size speed btype)
                | btype == BP = Bullet (x + speed) y size speed btype
                | otherwise = Bullet (x - speed) y size speed btype

deleteBullets :: Bullet -> Bool
deleteBullets bullet = (((bposX bullet) > -(fromIntegral screenWidth `div` 2)) && ((bposX bullet) < (fromIntegral screenWidth `div` 2)))

moveEnemies :: [Enemy] -> [Enemy]
moveEnemies enemies = filter (\enemy -> deleteEnemy enemy) (map moveEnemy enemies)

moveEnemy :: Enemy -> Enemy
moveEnemy (Enemy x y size speed lives) = Enemy (x - speed) y size speed lives

deleteEnemy :: Enemy -> Bool
deleteEnemy enemy = ((eposX enemy) > -(fromIntegral screenWidth `div` 2))

enemySpawner :: ([Enemy], [Int]) -> ([Enemy], [Int])
enemySpawner ( enemies , [] ) = (enemies, [])
enemySpawner ( enemies , [x]) = (enemies ++ waveSpawner (makeWaves x) 0, [])
enemySpawner ( enemies , (x:xs)) = (enemies ++ waveSpawner (makeWaves x) 0, xs)

makeWaves :: Int -> [Int]
makeWaves n
        | n - 5 >= 0 = 5 : makeWaves (n - 5)
        | otherwise = [n]

waveSpawner :: [Int] -> Int -> [Enemy]
waveSpawner [] _ = []
waveSpawner [wave] waveNb = (map (\n -> spawnEnemy waveNb (-(fromIntegral screenHeight `div` 2) + ((screenHeight `div` (wave + 1)) * n))) [1 .. wave])
waveSpawner (wave:waves) waveNb = (map (\n -> spawnEnemy waveNb (-(fromIntegral screenHeight `div` 2) + ((screenHeight `div` (wave + 1)) * n))) [1 .. wave]) ++ waveSpawner waves (waveNb + 1)

spawnEnemy :: Int -> Int -> Enemy 
spawnEnemy waveNb y = Enemy (enemySpawnX + (150 * waveNb)) y enemySize maxEnemySpeed 1

doCollision :: GameObjects -> (GameObjects, Int)
doCollision go = (filterLists go hitList, getLength hitList)
        where hitList = makeHitList go

makeHitList :: GameObjects -> (Int, [Enemy], [Bullet])
makeHitList go = (checkPlayerCollision go, checkEnemies go, checkBullets go)

getLength :: (Int, [Enemy], [Bullet]) -> Int
getLength (_, enemies, _) = length enemies

checkEnemies :: GameObjects -> [Enemy]
checkEnemies (GameObjects player ([], x) bullets) = []
checkEnemies go@(GameObjects player ([enemy], x) bullets) = (checkEnemyBullets enemy bullets) ++ (checkEnemyPlayer enemy player) 
checkEnemies go@(GameObjects player ((enemy:enemies), x) bullets) = checkEnemyBullets enemy bullets ++ (checkEnemyPlayer enemy player) ++ checkEnemies (GameObjects player (enemies, x) bullets)

checkEnemyBullets :: Enemy -> [Bullet] -> [Enemy]
checkEnemyBullets enemy [] = []
checkEnemyBullets enemy [bullet]
                | checkHitBox (eposX enemy) (eposY enemy) (esize enemy) (bposX bullet) (bposY bullet) (bsize bullet) = [enemy]
                | otherwise = []
checkEnemyBullets enemy (bullet:bullets) 
                | checkHitBox (eposX enemy) (eposY enemy) (esize enemy) (bposX bullet) (bposY bullet) (bsize bullet) = [enemy]
                | otherwise = checkEnemyBullets enemy bullets

checkBullets :: GameObjects -> [Bullet]
checkBullets go@(GameObjects player (enemies, x) [bullet])
            | (btype bullet) == BP = checkBulletEnemy bullet enemies
            | otherwise = checkBulletPlayer bullet player
checkBullets go@(GameObjects player (enemies, x) (bullet:bullets))
            | (btype bullet) == BP = checkBulletEnemy bullet enemies ++ checkBullets (GameObjects player (enemies, x) bullets)
            | otherwise = checkBulletPlayer bullet player ++ checkBullets (GameObjects player (enemies, x) bullets)

checkBulletEnemy :: Bullet -> [Enemy] -> [Bullet]
checkBulletEnemy bullet [] = []
checkBulletEnemy bullet [enemy]
            | checkHitBox (bposX bullet) (bposY bullet) (bsize bullet) (eposX enemy) (eposY enemy) (esize enemy) = [bullet]
            | otherwise = []
checkBulletEnemy bullet (enemy: enemies)
            | checkHitBox (bposX bullet) (bposY bullet) (bsize bullet) (eposX enemy) (eposY enemy) (esize enemy) = [bullet]
            | otherwise = checkBulletEnemy bullet enemies

checkBulletPlayer :: Bullet -> Player -> [Bullet]            
checkBulletPlayer bullet player
                    | checkHitBox (bposX bullet) (bposY bullet) (bsize bullet) (pposX player) (pposY player) (psize player) = [bullet]
                    | otherwise = []

checkEnemyPlayer :: Enemy -> Player -> [Enemy]            
checkEnemyPlayer enemy player
                    | checkHitBox (eposX enemy) (eposY enemy) (esize enemy) (pposX player) (pposY player) (psize player) = [enemy]
                    | otherwise = []

checkPlayerCollision :: GameObjects -> Int
checkPlayerCollision (GameObjects player (enemies, x) bullets) = length (filter (==True) ((map (\enemy -> checkHitBox (pposX player) (pposY player) (psize player) (eposX enemy) (eposY enemy) (esize enemy)) enemies) ++ [checkHitBox (pposX player) (pposY player) (psize player) (bposX bullet) (bposY bullet) (bsize bullet) | bullet <- bullets, (btype bullet) == BE ]))

-- x1, y1, size first object, x2, y2, size second object, returns true if objects collide
checkHitBox :: Int -> Int -> Int -> Int -> Int -> Int -> Bool 
checkHitBox x1 y1 s1 x2 y2 s2 = ((x1-x2)^2 + (y1-y2)^2) < (s1 + s2)^2

filterLists :: GameObjects -> (Int, [Enemy], [Bullet]) -> GameObjects
filterLists (GameObjects player (enemies, x) bullets) (playerHit, enemiesHit, bulletsHit) = GameObjects (decLives player playerHit) ((filter (\enemy -> (enemy `notElem` enemiesHit)) enemies), x) (filter (\bullet -> (bullet `notElem` bulletsHit)) bullets)

decLives :: Player -> Int -> Player
decLives (Player x y size speed lives) hit = Player x y size speed (lives - hit)

checkWinLoss :: GameState -> GameState
checkWinLoss gs@(GameState (GameObjects player (enemies, spawnCycle) _ ) _ _ _ _ _)
        | (plives $ player) <= 0 = gs {state = Loss}
        | (length spawnCycle) == 0 && (length enemies == 0) = gs {state = Won}
        | otherwise = gs

spawnRandomBullets :: [Bullet] -> [Enemy] -> StdGen -> [Bullet]
spawnRandomBullets bullets [] _ = bullets 
spawnRandomBullets bullets [enemy] rng
                            | 1 >= randomShoot = makeEnemyBullet enemy : bullets
                            | otherwise = bullets
spawnRandomBullets bullets (enemy: enemies) rng
                            | 1 >= randomShoot = makeEnemyBullet enemy : bullets ++ spawnRandomBullets bullets enemies rng
                            | otherwise = bullets ++ spawnRandomBullets bullets enemies rng

                            --(head (randomRs (0, randomShoot + 1) rng))
-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gs = return (inputKey e gs)

-- key up and down veranderen de speed wannee de key down is naar of -standaard speed of + standaard speed en wanneer de key dan los word gelaten word de speed weer op 0 gezed
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey k) state _ _) gs
    = case state of
        Down -> case k of
            KeyF1 -> toggleState gs
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

toggleState :: GameState -> GameState
toggleState gs@(GameState go state score elapsedTime enemyTime rng)
            | state == Start = gs {state = InGame}
            | state == InGame = gs {state = Pause}
            | state == Pause = gs {state = InGame}
            | otherwise = gs

checkPos :: Player -> Bool
checkPos player = (fromIntegral((pposY player) + (pspeed player)) < (fromIntegral screenHeight/2)) && (fromIntegral((pposY player) + (pspeed player)) > -(fromIntegral screenHeight/2))

-- Bullet x y type
makePlayerBullet :: Player -> Bullet
makePlayerBullet player = Bullet (pposX player + 10 ) (pposY player) bulletSize maxBulletSpeed BP

makeEnemyBullet :: Enemy -> Bullet 
makeEnemyBullet enemy = Bullet (eposX enemy - 10) (eposY enemy) bulletSize maxBulletSpeed BE