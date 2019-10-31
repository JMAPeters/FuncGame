-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view gs = return $ chooseView gs

chooseView :: GameState -> Picture
chooseView gs@(GameState gameObjects state score elapsedTime enemyTime rng)
   | state == Start = makeStartView
   | state == InGame = pictures [makeGameView gameObjects, makeUI gs]
   | state == Pause = makePauseView
   -- | state == Won = makeWinView gs
   -- | state == Loss = makeLossView gs
   | otherwise = color green (text "Error")

makeStartView :: Picture
makeStartView = translate (-500) (0) $ Scale 0.5 0.5 $ color green (text ("Shoot'em Up! Press F1 to start!"))

makeGameView :: GameObjects -> Picture
makeGameView go = pictures ([drawPlayer (player $ go), drawText (enemies $ go)] ++ drawEnemys (enemies $ go) ++ drawBullets (bullets $ go))

makeUI gs = pictures ([drawScore gs])

makePauseView :: Picture
makePauseView = translate (-500) (0) $ Scale 0.5 0.5 $ color green (text ("Pause! Press F1 to continue!"))

drawPlayer :: Player -> Picture
drawPlayer player = translate (fromIntegral (pposX $ player)) (fromIntegral (pposY $ player)) $ color blue $ circleSolid (fromIntegral(psize player))

drawEnemys :: ([Enemy], [Int]) -> [Picture]
drawEnemys (enemies, _) = map drawEnemy enemies

drawEnemy :: Enemy -> Picture
drawEnemy enemy = translate (fromIntegral (eposX $ enemy)) (fromIntegral (eposY $ enemy)) $ color red $ circleSolid (fromIntegral(esize enemy))

drawBullets :: [Bullet] -> [Picture]
drawBullets bullets = map drawBullet bullets 

drawBullet :: Bullet -> Picture
drawBullet bullet = translate (fromIntegral (bposX $ bullet)) (fromIntegral (bposY $ bullet)) $ color green $ circleSolid (fromIntegral(bsize bullet))

drawScore :: GameState -> Picture
drawScore gs = Translate (5 - fromIntegral screenWidth / 2) (fromIntegral screenHeight / 2 + 5) $ Scale 0.2 0.2 $ Color white $ text ("Score: " ++ (show (score gs)))

drawText (enemies, _) = color green (text (show(length enemies)))