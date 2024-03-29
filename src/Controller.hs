-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gs@(GameState gameObjects state score elapsedTime lastEnemySpawn rng hs) 
    | state == Won
          = do writeFile "textFiles/hs.txt" (show hs)
               return $ gs
    | state /= InGame
        = return $ gs
    | elapsedTime + secs > nO_SECS_BETWEEN_CYCLES
        = return $ checkWinLoss (doAllSteps gs (lastEnemySpawn + 1) )
    | otherwise = 
        return $ gs {elapsedTime = elapsedTime + secs}
        
-- | Relys on doMainSteps to update on every frame
doAllSteps gs n
        | n >= enemySpawnTimer = spawnRandomBullets (unTuple gs 0 (doMainSteps ((gameObjects $ gs) { enemies = enemySpawner(enemies $ gameObjects $ gs)}) (rng $ gs)))
        | otherwise = spawnRandomBullets (unTuple gs n (doMainSteps (gameObjects $ gs) (rng $ gs)))

unTuple :: GameState -> Int -> (GameObjects, Int) -> GameState
unTuple (GameState gameObjects state score elapsedTime lastEnemySpawn rng hs) n (go, points) = (GameState go state (score+points) elapsedTime n rng hs)

-- | Checks all Game Entities for change every step
doMainSteps :: GameObjects -> StdGen -> (GameObjects, Int)
doMainSteps (GameObjects player (enemies, x) bullets animations) rng = checkCollision (GameObjects (movePlayer player) ((moveEnemies enemies), x) (moveBullets bullets) (playAnimation animations) )

-- |Player movement, based on player speed. 
movePlayer :: Player -> Player
movePlayer player@(Player x y size speed lives)
                | checkPos player = Player x (y + speed) size speed lives
                | otherwise = player

-- | Updates all bullets in de bullet list
moveBullets :: [Bullet] -> [Bullet]
moveBullets bullets = filter (\bullet -> deleteBullets bullet) (map moveBullet bullets)

-- | Move a single bullet
moveBullet :: Bullet -> Bullet 
moveBullet (Bullet x y size speed btype)
                | btype == BP = Bullet (x + speed) y size speed btype
                | otherwise = Bullet (x - speed) y size speed btype

-- | Delete bullets from bullet list when they get out of bound (screen size)
deleteBullets :: Bullet -> Bool
deleteBullets bullet | (btype bullet) == BP = ((bposX bullet) > -(fromIntegral screenWidth `div` 2))
                     | otherwise = ((bposX bullet) < (fromIntegral screenWidth `div` 2))

-- | Update entire enemy list for movement
moveEnemies :: [Enemy] -> [Enemy]
moveEnemies enemies = filter (\enemy -> deleteEnemy enemy) (map moveEnemy enemies)

-- | Move single enemy entity 
moveEnemy :: Enemy -> Enemy
moveEnemy (Enemy x y size speed lives) = Enemy (x - speed) y size speed lives

-- | Delete enemy when out of bound (screen size)
deleteEnemy :: Enemy -> Bool
deleteEnemy enemy = ((eposX enemy) > -(fromIntegral screenWidth `div` 2))

-- | Enemy Spawn pattern, is passed on the SpawnEnemyCycle
enemySpawner :: ([Enemy], [Int]) -> ([Enemy], [Int])
enemySpawner ( enemies , [] ) = (enemies, [])
enemySpawner ( enemies , [x]) = (enemies ++ waveSpawner (makeWaves x) 0, [])
enemySpawner ( enemies , (x:xs)) = (enemies ++ waveSpawner (makeWaves x) 0, xs)

-- | Generates the enemy attact waves from SpawnEnemyCycle
makeWaves :: Int -> [Int]
makeWaves n
        | n - 5 >= 0 = 5 : makeWaves (n - 5)
        | otherwise = [n]

-- | One single wave          
waveSpawner :: [Int] -> Int -> [Enemy]
waveSpawner [] _ = []
waveSpawner [wave] waveNb = (map (\n -> spawnEnemy waveNb (-(fromIntegral screenHeight `div` 2) + ((screenHeight `div` (wave + 1)) * n))) [1 .. wave])
waveSpawner (wave:waves) waveNb = (map (\n -> spawnEnemy waveNb (-(fromIntegral screenHeight `div` 2) + ((screenHeight `div` (wave + 1)) * n))) [1 .. wave]) ++ waveSpawner waves (waveNb + 1)

-- | Spawns one single enemy
spawnEnemy :: Int -> Int -> Enemy 
spawnEnemy waveNb y = Enemy (enemySpawnX + (150 * waveNb)) y enemySize maxEnemySpeed 1

-- | Checks for collision between two game entities, based up on the #enemies hits the Interger in the tuple is passed on as the score.
checkCollision :: GameObjects -> (GameObjects, Int)
checkCollision go@(GameObjects player (enemies, x) bullets _) = filterHitList go (checkBPEnemy enemies bullets) (checkPlayerBEEnemy player enemies bullets)

-- | Creates a new tuple based upon all the game entities who got hit. For the enemy hit list the length will determine the score
filterHitList :: GameObjects -> [(Bullet, Enemy)] -> ([(Player, Enemy)], [(Player, Bullet)]) -> (GameObjects, Int)
filterHitList (GameObjects player (enemies, x) bullets animations) bulletEnemy (playerEnemy, playerBullet) = (GameObjects (decLives player (length playerEnemy + length playerBullet)) ((filter (\enemy -> (enemy `notElem` enemiesHit)) enemies), x) (filter (\bullet -> (bullet `notElem` bulletsHit)) bullets) ((makeAnimations enemiesHit) ++ animations), (length enemiesHit))
        where   enemiesHit = map snd bulletEnemy ++ map snd playerEnemy
                bulletsHit = map fst bulletEnemy ++ map snd playerBullet

-- | Takes in the Enemy Hit list and for the enemies who got hit creates an animation
makeAnimations :: [Enemy] -> [Animation]
makeAnimations enemies = map(\enemy -> Animation (eposX enemy) (eposY enemy) 0) enemies

-- | Play the animation 
playAnimation :: [Animation] -> [Animation]
playAnimation [] = []
playAnimation ((Animation x y size):animations)
                | (size) < animationSize = (Animation x y (size + 5)) : playAnimation animations
                | otherwise = [] ++ playAnimation animations

-- | Check if player bullets have collision with the enemies         
checkBPEnemy :: [Enemy] -> [Bullet] -> [(Bullet, Enemy)]
checkBPEnemy enemies bullets = map (\index -> listOfBulletEnemy !! index)(listOfIndex listOfHit)
        where   listOfBulletEnemy = [(bullet,enemy) | enemy <- enemies, bullet <- bullets, (btype $ bullet) == BP ]
                listOfHit = map checkBulletEnemy listOfBulletEnemy
                listOfIndex list = elemIndices True list
                checkBulletEnemy (bullet,enemy) = checkHitBox (bposX bullet) (bposY bullet) (bsize bullet) (eposX enemy) (eposY enemy) (esize enemy)

-- | Check if the player collides with enemy or enemy bullets      
checkPlayerBEEnemy :: Player -> [Enemy] -> [Bullet] -> ([(Player, Enemy)], [(Player, Bullet)])
checkPlayerBEEnemy player enemies bullets = (map (\index -> listOfEnemy !! index)(listOfIndex listOfHitEnemy), map (\index -> listOfBullet !! index)(listOfIndex listOfHitBullet)) 
        where   listOfEnemy = [(player,enemy) | enemy <- enemies]
                listOfHitEnemy = map checkPlayerEnemy listOfEnemy                
                listOfBullet = [(player,bullet) | bullet <- bullets, (btype $ bullet) == BE]
                listOfHitBullet = map checkPlayerBullet listOfBullet
                listOfIndex = elemIndices True
                checkPlayerEnemy (player,enemy) = checkHitBox (pposX player) (pposY player) (psize player) (eposX enemy) (eposY enemy) (esize enemy)
                checkPlayerBullet (player,bullet) = checkHitBox (pposX player) (pposY player) (psize player) (bposX bullet) (bposY bullet) (bsize bullet)

-- | Check if two objects collied (x1, y1, size first object, x2, y2, size second object, returns true if objects collide)
checkHitBox :: Int -> Int -> Int -> Int -> Int -> Int -> Bool 
checkHitBox x1 y1 s1 x2 y2 s2 = ((x1-x2)^2 + (y1-y2)^2) < (s1 + s2)^2

-- | Subtrack a Player life when hit by a enemy bullet
decLives :: Player -> Int -> Player
decLives (Player x y size speed lives) hit = Player x y size speed (lives - hit)

-- | Check if the player is still alive, if so check the enemy spawnCycle to know if to keep playing
checkWinLoss :: GameState -> GameState
checkWinLoss gs@(GameState (GameObjects player (enemies, spawnCycle) _ _ ) _ score _ _ _ hs)
        | (plives $ player) <= 0 = gs {state = Loss}
        | (length spawnCycle) == 0 && (length enemies == 0) = gs {state = Won, highScore = checkHighScore score hs }
        | otherwise = gs

-- | Check if achieved score is higher then actual high score.        
checkHighScore :: Int -> Int -> Int
checkHighScore score hs | score > hs = score
                        | otherwise = hs

-- | Create enemy shoot behaviour based on randomness                        
spawnRandomBullets :: GameState -> GameState
spawnRandomBullets gs@(GameState (GameObjects _ ([], spawnCycle) _ _ ) _ _ _ _ _ _) = gs
spawnRandomBullets gs@(GameState gameobjects@(GameObjects player (enemies, spawnCycle) bullets animations ) state score elapsedTime lastEnemySpawn rng hs) = let (enemyBullets, newRng) = randomBullets enemies rng
                                                                                                                                                          in GameState (GameObjects player (enemies,spawnCycle) (bullets ++ enemyBullets) animations) state score elapsedTime lastEnemySpawn newRng hs
                                                                                                                                                          
randomBullets :: [Enemy] -> StdGen -> ([Bullet], StdGen)
randomBullets enemies rng = let (numb, newRng) = randomR (0, randomShoot) rng
                            in ([makeEnemyBullet enemy | enemy <- enemies, numb >= randomShoot], newRng)

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gs = return (inputKey e gs)


-- | KeyUp & KeyDown change speed when key down is pressed, when released speed will be set to default (0)
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

-- | Check game state to start game of switch from play to pause
toggleState :: GameState -> GameState
toggleState gs@(GameState _ state _ _ _ _ _)
            | state == Start = gs {state = InGame}
            | state == InGame = gs {state = Pause}
            | state == Pause = gs {state = InGame}
            | otherwise = gs

-- | Check if player is in the screen       
checkPos :: Player -> Bool
checkPos player = (fromIntegral((pposY player) + (pspeed player)) < (fromIntegral screenHeight/2)) && (fromIntegral((pposY player) + (pspeed player)) > -(fromIntegral screenHeight/2))

-- | Create Player Bullet x y type
makePlayerBullet :: Player -> Bullet
makePlayerBullet player = Bullet (pposX player + 30 ) (pposY player) bulletSize maxBulletSpeed BP

-- | Create Enemy Bullet
makeEnemyBullet :: Enemy -> Bullet 
makeEnemyBullet enemy = Bullet (eposX enemy) (eposY enemy) bulletSize maxBulletSpeed BE
