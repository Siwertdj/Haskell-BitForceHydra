{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use list literal" #-}
-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: MainState -> IO Picture
view = return . viewPure

viewPure :: MainState -> Picture
viewPure (MMenu menu)
  = viewMenu menu
viewPure (MGame gstate)
  = viewGame gstate
viewPure _
  = viewText "closing game" (0,0) (0,0)

viewMenu :: MenuState -> Picture
viewMenu (MenuState (Menu options pointer) game _)
  = pictures $
      --something       :
        map (viewMenuOption pointer) options
    ++ [Graphics.Gloss.color white  $ scale 64 64 $ rectangleWire (rightBound/32)(upperBound/32)] 
    ++ [viewGame game]

viewGame :: GameState -> Picture
viewGame (GameState (World player entities _ _ text ) score _ _)
  = pictures $
        [viewText (show score) (0,upperBound/2) (1,1)]
    ++  (map drawEntities entities)
    ++  [drawEntities player]
    ++  [viewText text (leftBound,0) (0.2,0.2)]


viewMenuOption :: Int -> MenuOption -> Picture
viewMenuOption currentIndex (MenuOption text optionIndex _)
  = pictures $
      (viewText text  (leftBound + 50, upperBound - spacing * (fromIntegral optionIndex))   (scale, scale))
    : (viewText ">"   (leftBound + 10, upperBound - spacing * (fromIntegral currentIndex))  (scale, scale))
    : []
  where 
    spacing = 30 :: Float
    scale = 0.2

viewText :: String -> (Float,Float) -> (Float, Float) -> Picture
viewText cs (x,y) (sx,sy)=  translate x y $ Graphics.Gloss.color white $ scale sx sy $ text (show cs)




drawEntities :: Entity -> Picture
--drawShips (ShipObject _ (locX,locY) width height colr) = translate locX locY $ Graphics.Gloss.color colr $ rectangleSolid width height
drawEntities (Entity eType faction (width, height) (Movement (locX,locY)  _ angle _ _))
  = translate locX locY $ rotate angle $ Graphics.Gloss.color (drawColor eType faction) $ rectangleSolid width height
    where
      drawColor :: EntityType Float -> Faction -> Color
      drawColor (Bullet _) _ = yellow
      drawColor (Obstacle _) _ = blue
      drawColor (Destruction _ _) _ = orange              --does this work properly?
      drawColor _ fac = case fac of
                                  Player  -> green
                                  Enemy   -> red
                                  Neutral -> white

                                  