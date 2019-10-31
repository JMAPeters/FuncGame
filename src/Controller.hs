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
        = return $ doAllSteps gs (lastEnemySpawn + 1) 
    | otherwise = 
        return $ gs {elapsedTime = elapsedTime + secs}
-- let in
doAllSteps :: GameState -> Int -> GameState
doAllSteps gs@(GameState gameObjects state score elapsedTime lastEnemySpawn rng) n
            | n >= enemySpawnTimer = (GameState (doMainSteps ((gameObjects) { enemies = enemySpawner(enemies $ gameObjects )})) state score elapsedTime 0 rng )
            | otherwise = (GameState (doMainSteps gameObjects) state score elapsedTime n rng)

-- return tuple van GameObject en Score 
doMainSteps :: GameObjects -> (GameObjects, Int)
doMainSteps (GameObjects player (enemies, x) bullets) = doCollision (GameObjects (movePlayer player) ((moveEnemies enemies), x) (moveBullets {-(spawnRandomBullets-} bullets {-enemies)-}))

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
doCollision go = (filterLists go (makeHitList go), getLengthEnemy (makeHitList go))

makeHitList :: GameObjects -> ([Enemy], [Bullet])
makeHitList go = (checkEnemies go, checkBullets go)

getLengthEnemy :: ([Enemy], [Bullet]) -> Int
getLengthEnemy (enemies, _) = length enemies

checkEnemies :: GameObjects -> [Enemy]
checkEnemies go@(GameObjects player ([enemy], x) bullets) = checkEnemyBullets enemy bullets 
checkEnemies go@(GameObjects player ((enemy:enemies), x) bullets) = checkEnemyBullets enemy bullets ++ checkEnemies (GameObjects player (enemies, x) bullets)


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

checkBulletPlayer b p = []

-- x1, y1, size first object, x2, y2, size second object, returns true if objects collide
checkHitBox :: Int -> Int -> Int -> Int -> Int -> Int -> Bool 
checkHitBox x1 y1 s1 x2 y2 s2 = ((x1-x2)^2 + (y1-y2)^2) < (s1 + s2)^2

filterLists :: GameObjects -> ([Enemy], [Bullet]) -> GameObjects
filterLists (GameObjects player (enemies, x) bullets) (enemiesHit, bulletsHit) = GameObjects player ((filter (\enemy -> (enemy `notElem` enemiesHit)) enemies), x) (filter (\bullet -> (bullet `notElem` bulletsHit)) bullets)

spawnRandomBullets :: [Bullet] -> [Enemy] -> [Bullet]
spawnRandomBullets bullets [] = bullets 
spawnRandomBullets bullets [enemy] 
                            | 1 >= randomShoot = makeEnemyBullet enemy : bullets
                            | otherwise = bullets
spawnRandomBullets bullets (enemy: enemies) 
                            | 1 >= randomShoot = makeEnemyBullet enemy : bullets ++ spawnRandomBullets bullets enemies
                            | otherwise = bullets ++ spawnRandomBullets bullets enemies
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