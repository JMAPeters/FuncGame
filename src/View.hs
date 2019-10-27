-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model



view :: GameState -> IO Picture
view gs = return $ makeView (gameObjects $ gs)

makeView :: GameObjects -> Picture
makeView go = pictures ([drawPlayer (player $ go), drawText (enemies $ go)] ++ drawEnemys (enemies $ go) ++ drawBullets (bullets $ go))

drawPlayer :: Player -> Picture
drawPlayer player = translate (fromIntegral (pposX $ player)) (fromIntegral (pposY $ player)) $ color blue $ circleSolid 30

drawEnemys :: ([Enemy], [Int]) -> [Picture]
drawEnemys (enemies, _) = map drawEnemy enemies

drawEnemy :: Enemy -> Picture
drawEnemy enemy = translate (fromIntegral (eposX $ enemy)) (fromIntegral (eposY $ enemy)) $ color red $ circleSolid 30

drawBullets :: [Bullet] -> [Picture]
drawBullets bullets = map drawBullet bullets 

drawBullet :: Bullet -> Picture
drawBullet bullet = translate (fromIntegral (bposX $ bullet)) (fromIntegral (bposY $ bullet)) $ color green $ circleSolid 10


drawText (enemies, _) = color green (text (show(length enemies)))