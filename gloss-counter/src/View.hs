-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: MainState -> IO Picture
view = return . viewPure

viewPure :: MainState -> Picture
viewPure (MMenu _) = undefined
viewPure (MGame (GameState (World player entities _ _ ) _ _ _))
  = pictures (
        map drawEntities entities
    ++  [ drawEntities player]
    )



drawEntities :: Entity -> Picture
--drawShips (ShipObject _ (locX,locY) width height colr) = translate locX locY $ Graphics.Gloss.color colr $ rectangleSolid width height
drawEntities (Entity eType faction (locX,locY) (width, height) _ angle _)
  = rotate angle $ translate locX locY $ Graphics.Gloss.color (drawColor eType faction) $ rectangleSolid width height
    where
      drawColor :: EntityType Float -> Faction -> Color
      drawColor (Bullet _) _ = yellow
      drawColor (Obstacle _) _ = blue
      drawColor _ fac = case fac of
                                  Player  -> green
                                  Enemy   -> red
                                  Neutral -> white

                                  