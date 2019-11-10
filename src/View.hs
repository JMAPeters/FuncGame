-- | This module defines how to turn
module View where

import Graphics.Gloss
import Model
import Controller
import System.Random

view :: GameState -> IO Picture
view gs = return $ chooseView gs

chooseView :: GameState -> Picture
chooseView gs@(GameState gameObjects state score elapsedTime enemyTime rng hs)
   | state == Start = pictures [makeStartView, makeInfo, makeHS hs]
   | state == InGame = pictures [makeGameView gameObjects rng, makeUI gs]
   | state == Pause = makePauseView gs
   | state == Won = pictures [makeWinView gs, makeHS hs]
   | state == Loss = makeLossView gs
   | otherwise = color green (text "Error")

makeStartView :: Picture
makeStartView = translate (-500) (0) $ Scale 0.5 0.5 $ color green (text ("Shoot'em Up! Press F1 to start!"))

makeInfo :: Picture
makeInfo = translate (-500) (-50) $ Scale 0.2 0.2 $ color green (text ("Move Up and Down with the arrow key and shoot with the space bar"))

makeHS :: Int -> Picture
makeHS hs = translate (-200) (-150) $ Scale 0.5 0.5 $ color green (text ("Highscore: " ++ (show hs)))

makeGameView :: GameObjects -> StdGen -> Picture
makeGameView go rng = pictures ([drawPlayer (player $ go){-, drawText (r (0, randomShoot) rng)-}] ++ drawEnemys (enemies $ go) ++ drawBullets (bullets $ go) ++ drawAnimations (animations $ go))

makeUI gs = pictures ([drawScore gs, drawLives gs])

makePauseView :: GameState -> Picture
makePauseView gs = pictures ([drawPauseText, drawScoreMenu gs])

makeWinView :: GameState -> Picture
makeWinView gs = pictures ([drawWinText, drawScoreMenu gs])

makeLossView :: GameState -> Picture
makeLossView gs = pictures ([drawLossText, drawScoreMenu gs])

drawPauseText :: Picture
drawPauseText = translate (-500) (0) $ Scale 0.5 0.5 $ color green (text ("Pause! Press F1 to continue!"))

drawScoreMenu :: GameState -> Picture
drawScoreMenu gs = translate (-100) (-100) $ Scale 0.5 0.5 $ color green (text ("Score: " ++ (show (score gs))))

drawWinText :: Picture
drawWinText = translate (-100) (0) $ Scale 0.5 0.5 $ color green (text ("You Won!"))

drawLossText :: Picture
drawLossText = translate (-100) (0) $ Scale 0.5 0.5 $ color green (text ("You Lost!"))

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

drawAnimations :: [Animation] -> [Picture]
drawAnimations animations = map drawAnimation animations

drawAnimation :: Animation -> Picture
drawAnimation animation = translate (fromIntegral (aposX $ animation)) (fromIntegral (aposY $ animation)) $ color orange $ circle (fromIntegral(asize animation))

drawScore :: GameState -> Picture
drawScore gs = Translate (-600) (-350) $ Scale 0.2 0.2 $ Color white $ text ("Score: " ++ (show (score gs)))

drawLives :: GameState -> Picture
drawLives gs = Translate (-600) (-320) $ Scale 0.2 0.2 $ Color white $ text ("Lives: " ++ (show (plives $ player $ gameObjects $ gs)))

drawText (x,_) = color green (text (show x))