-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Data (ConstrRep(FloatConstr))

-- | Handle one iteration of the game
step :: Float -> MainState -> IO MainState
step _ mstate@(MMenu (MenuState menu emptyGame))                                   --  IS IN MENU
  = return mstate
step secs mstate@(MGame (GameState world score elapsedTime keys))                  --  IS IN GAME
  = -- Just update the elapsed time
    do
      newWorld <- updateWorld world elapsedTime keys
      return $ MGame (GameState
        newWorld
        score
        (elapsedTime + secs)
        keys
        )


updateWorld :: World -> Float -> KeysOfInput -> IO World
updateWorld (World player entities speed spawnIncrement) time keys =
    do
      newEnemy <- spawnEnemy
      return $ World
                ( -- Update Player --
                  handlePlayer (fst $ canShoot' player) keys
                )
                ( -- Update Entities --
                  filter (not . isOutOfBounds)(
                  (if (spawnIncrement - time) <= 0 then newEnemy : updatedEntities else updatedEntities)
                  ++
                  ([Entity (Bullet 1) Player (findEntity player) (10,10) (-10) (0,0) | snd (canShoot' player)])
                  ++
                  ([Entity (Bullet 1) fac eLoc (10,10) 10 (0,0) | (enemy@(Entity _ fac eLoc _ _ _), shooting) <- entitiesCanShoot, shooting && fac == Enemy])
                  )
                )
                (-- Update scrollspeed --
                speed
                )
                ( -- Update spawnIncrement -- 
                  if time > spawnIncrement then time + initialSpawnIncrement else spawnIncrement -- next spawn time is upped by 5 seconds (the increment) when the elapsedTime passes its current value, thus every 5 seconds something will spawn.
                )     
      where
        canShoot' a = shootTimer a time keys
        entitiesCanShoot = map canShoot' (movingEntities entities speed)
        updatedEntities = map fst (entitiesCanShoot)


spawnEnemy :: IO Entity
spawnEnemy =
    do
      let amountOfEnemyTypes = length enemyTypes -1
      randomLocation  <- randomRIO(leftBound, rightBound) :: IO Float
      randomSelector  <- randomRIO(0, amountOfEnemyTypes) :: IO Int
      let randomEnemy = enemyTypes !! randomSelector
      return $ randomEnemy {location = (randomLocation, upperBound)}


handlePlayer :: Entity -> KeysOfInput -> Entity
handlePlayer player@(Entity etype faction loc hitbox speed angle) keys
  = Entity
      etype
      faction
      (movingPlayer loc hitbox keys)         -- change location
      hitbox
      speed
      angle



-- HANDLE MOVING --
movingPlayer :: Location -> Hitbox -> KeysOfInput -> Location
movingPlayer (locX,locY) (width, height) (Keys wIn aIn sIn dIn _)
  | wIn && not sIn && locY < (upperBound-height/2) = (locX, locY + playerSpeed)
  | sIn && not wIn && locY > (lowerBound+height/2) = (locX, locY - playerSpeed)
  | aIn && not dIn && locX > (leftBound+width/2) = (locX - playerSpeed, locY)
  | dIn && not aIn && locX < (rightBound-width/2) = (locX + playerSpeed, locY)
  | otherwise = (locX, locY)

-- check of entitiy een enemy is, zo ja, dan handle hun movement anders. 
-- Sommigen bewegen zijwaarts, de speler volgend, anderen in een (bv) golfpatroon
movingEntities :: [Entity] -> Float -> [Entity]
movingEntities entities scrollSpeed = map (moveEntities scrollSpeed) entities

moveEntities :: Float -> Entity -> Entity
moveEntities scrollSpeed entity@(Entity eType faction location hitbox speed angle)
  = (entity \/ scrollSpeed) \/ speed

-- If out of bounds == True
isOutOfBounds :: Entity -> Bool
isOutOfBounds (Entity _ _ (x,y) _ _ _) = y < lowerBound || y > upperBound || x < leftBound || x > rightBound


-- HANDLE SHOOTING --
isShooting :: KeysOfInput -> Bool
isShooting (Keys _ _ _ _ spaceIn) = spaceIn

canShoot :: Entity -> Float -> Bool
canShoot e@(Entity (Shooter _ (inc, next)) fac _ _ _ _) time = case fac of
                                                  Player  -> next - time <= 0
                                                  Enemy   -> next - time <= 0
                                                  _       -> False              -- Neutrals

shootTimer :: Entity -> Float -> KeysOfInput -> (Entity, Bool)
shootTimer e@(Entity (Shooter hp (inc, next)) Player loc hb spd ang) time keys = 
  if canShoot e time && isShooting keys
    then (Entity (Shooter hp (inc, next + inc)) Player loc hb spd ang, True)
    else (e,False)
shootTimer e@(Entity (Shooter hp (inc, next)) Enemy loc hb spd ang) time _ = 
  if canShoot e time
    then (Entity (Shooter hp (inc, next + inc)) Enemy loc hb spd ang, True)
    else (e,False)
shootTimer e _ _ = (e, False)




-- Collision? --
-- it must exclude itself out of the list before passing it to the function, otherwise collision is always there
checkCollision :: Entity -> [Entities] -> Bool
checkCollision = undefined




findEntity :: Entity -> Location
findEntity (Entity _ _ loc _ _ _) = loc

class Move a where      -- (Float, Float) 
  (/\) , (\/), (~>), (<~) :: a -> Float -> a          -- /\ is up, \/ is down

instance Move Entity where
  entity /\ y = entity { location = (fst $ findEntity entity, snd (findEntity entity) + y) }
  entity \/ y = entity { location = (fst $ findEntity entity, snd (findEntity entity) - y) }
  entity ~> x = entity { location = (fst (findEntity entity) + x, snd $ findEntity entity) }
  entity <~ x = entity { location = (fst (findEntity entity) - x, snd $ findEntity entity) }

{-
instance Move Location where
  location /\ y = toLocation (fst (fromLocation location), snd (fromLocation location) + y)
  location \/ y = toLocation (fst (fromLocation location), snd (fromLocation location) - y)
  location ~> x = toLocation (fst (fromLocation location) + x, snd (fromLocation location))
  location <~ x = toLocation (fst (fromLocation location) + y, snd (fromLocation location))
-}


--extractPlayer :: World -> Entity
--extractPlayer (World p _ _ _) = p

{-
fromLocation :: Location -> (Float, Float)
fromLocation (locX, locY) = (locX, locY)

toLocation :: (Float, Float) -> Location
toLocation (locX, locY) = (locX, locY)
-}





















-- | Handle user input
input :: Event -> MainState -> IO MainState
input e mstate = return (inputKey e mstate)

inputKey :: Event -> MainState -> MainState
-- Moving input
inputKey (EventKey (Char 'w') Down _ _) mstate@(MGame (GameState w s t keys))
  = MGame (GameState w s t (keys {keyPressedW = True}))
inputKey (EventKey (Char 'w') Up _ _) mstate@(MGame (GameState w s t keys))
  = MGame (GameState w s t (keys {keyPressedW = False}))
inputKey (EventKey (Char 'a') Down _ _) mstate@(MGame (GameState w s t keys))
  = MGame (GameState w s t (keys {keyPressedA = True}))
inputKey (EventKey (Char 'a') Up _ _) mstate@(MGame (GameState w s t keys))
  = MGame (GameState w s t (keys {keyPressedA = False}))
inputKey (EventKey (Char 's') Down _ _) mstate@(MGame (GameState w s t keys))
  = MGame (GameState w s t (keys {keyPressedS = True}))
inputKey (EventKey (Char 's') Up _ _) mstate@(MGame (GameState w s t keys))
  = MGame (GameState w s t (keys {keyPressedS = False}))
inputKey (EventKey (Char 'd') Down _ _) mstate@(MGame (GameState w s t keys))
  = MGame (GameState w s t (keys {keyPressedD = True}))
inputKey (EventKey (Char 'd') Up _ _) mstate@(MGame (GameState w s t keys))
  = MGame (GameState w s t (keys {keyPressedD = False}))

-- Shooting input
inputKey (EventKey (SpecialKey KeySpace) Down _ _) mstate@(MGame (GameState w s t keys))
  = MGame (GameState w s t (keys {keyPressedSpace = True}))
inputKey (EventKey (SpecialKey KeySpace) Up _ _) mstate@(MGame (GameState w s t keys))
  = MGame (GameState w s t (keys {keyPressedSpace = False}))

-- Lack of input
inputKey _ mstate = mstate -- Otherwise keep the same


