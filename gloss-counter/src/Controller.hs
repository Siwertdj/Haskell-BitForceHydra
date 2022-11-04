{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Data (ConstrRep(FloatConstr))
import Data.List
import Graphics.Gloss.Geometry.Angle (radToDeg, normalizeAngle)

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
updateWorld (World player entities speed spawnIncrement overlay) time keys =
    do
      newEnemy <- spawnEnemy
      return $ World
                ( -- Update Player --
                  collideWithAll 
                  (handlePlayer (fst $ canShoot' player) keys) 
                  entities
                )
                ( -- Update Entities --
                   map (allignToPlayer player) $ filter (not . isOutOfBounds) $ filter (not.isDestroyed) (
                  (if (spawnIncrement - time) <= 0 then map (changeAngle player) (newEnemy : updatedEntities) else map (changeAngle player) updatedEntities)
                  ++
                  ([Entity (Bullet 30) Player (findEntity player) (10,10) (-10) 0 (-1) | snd (canShoot' player)])
                  ++
                  ([(Entity (Bullet 1) fac eLoc (10,10) 10 angle (-1))| (enemy@(Entity _ fac eLoc _ _ angle _), shooting) <- entitiesCanShoot, shooting && fac == Enemy])
                  )
                )
                
                -- Update scrollspeed --
                speed
                ( -- Update spawnIncrement -- 
                  if time > spawnIncrement then time + initialSpawnIncrement else spawnIncrement -- next spawn time is upped by 5 seconds (the increment) when the elapsedTime passes its current value, thus every 5 seconds something will spawn.
                )
                (
                  ("Number of shooters: " ++ show (length (filter (isOfEType (Shooter 0 (0,0))) entities)) 
                   ++ "   " ++ 
                  "Number of bullets: " ++ show (length (filter (isOfEType (Bullet 0)) entities))  
                   ++ "   " ++ 
                   "Status: " ++ (show (getPlayerStatus player))
                   -- ++ "   " ++ 
                   --"Player loc: " ++ (show $ fst (findEntity player)) ++ ", " ++ (show $ snd (findEntity player))
                  )
                )     
      where
        canShoot' a = shootTimer a time keys
        entitiesCanShoot = map canShoot' (movingEntities entities speed)
        updatedEntities = map fst entitiesCanShoot


spawnEnemy :: IO Entity
spawnEnemy =
    do
      let amountOfEnemyTypes = length enemyTypes -1
      randomLocation  <- randomRIO(leftBound, rightBound) :: IO Float
      randomSelector  <- randomRIO(0, amountOfEnemyTypes) :: IO Int
      let randomEnemy = enemyTypes !! randomSelector
      return $ randomEnemy {location = (randomLocation, upperBound)}


handlePlayer :: Entity -> KeysOfInput -> Entity
handlePlayer player@(Entity etype faction loc hitbox speed angle mID) keys
  = Entity
      etype
      faction
      (movingPlayer loc hitbox keys)         -- change location
      hitbox
      speed
      angle
      mID



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
moveEntities scrollSpeed entity@(Entity eType faction location hitbox speed angle mID)
  = (entity \/ scrollSpeed) \/ speed

moveBullet :: Entity -> Entity
moveBullet e@(Entity (Bullet hp) Enemy (ex,ey) hb spd angle 1) = 
  (Entity (Bullet hp) Enemy (ex,ey) hb spd angle 1)

  where 


changeAngle :: Entity -> Entity -> Entity
changeAngle p@(Entity _ Player (px,py) _ _ _ _) e@(Entity eType Enemy (ex,ey) hb spd angle 1) =
  Entity eType Enemy (ex,ey) hb spd newAngle 1
 where distX = px - ex
       distY = py - ey
       newAngle = radToDeg nAngle
       nAngle = normalizeAngle (atan (distX/distY))
changeAngle p e = e

allignToPlayer :: Entity -> Entity -> Entity
allignToPlayer p@(Entity _ Player (px,py) _ _ _ _) e@(Entity eType Enemy (ex,ey) hb spd angle 2) 
  | px > ex - spd = Entity eType Enemy (ex - spd,ey) hb spd angle 2
  | px < ex + spd = Entity eType Enemy (ex + spd,ey) hb spd angle 2
  | otherwise = e
allignToPlayer p e = e

-- If out of bounds == True
isOutOfBounds :: Entity -> Bool
isOutOfBounds (Entity _ _ (x,y) _ _ _ _) = y < lowerBound || y > upperBound || x < leftBound || x > rightBound


-- HANDLE SHOOTING --
isShooting :: KeysOfInput -> Bool
isShooting (Keys _ _ _ _ spaceIn) = spaceIn

canShoot :: Entity -> Float -> Bool
canShoot e@(Entity (Shooter _ (inc, next)) fac _ _ _ _ _) time = case fac of
                                                  Player  -> next - time <= 0
                                                  Enemy   -> next - time <= 0
                                                  _       -> False              -- Neutrals
canShoot _ _ = False

-- The return type of (Entity, Bool) represents (<Entity in question>, <Can it shoot?>)
shootTimer :: Entity -> Float -> KeysOfInput -> (Entity, Bool)
shootTimer e@(Entity (Shooter hp (inc, next)) Player loc hb spd ang mID) time keys =
  if canShoot e time && isShooting keys
    then (Entity (Shooter hp (inc, time + inc)) Player loc hb spd ang mID, True)
    else (e,False)
shootTimer e@(Entity (Shooter hp (inc, next)) Enemy loc hb spd ang mID) time _ =
  if canShoot e time
    then (Entity (Shooter hp (inc, time + inc)) Enemy loc hb spd ang mID, True)
    else (e,False)
shootTimer e _ _ = (e, False)




-- Collision --
-- it must exclude itself out of the list before passing it to the function, otherwise collision is always there
collision :: [Entity] -> [Entity]
collision entities = [ collideWithAll (entities!!(n-1)) (excludeSubject n entities) |  n <- [1..(length entities)]]
  where 
    excludeSubject :: Int -> [a] -> [a]
    excludeSubject n xs = take (n-1) xs ++ (drop n xs)

collideWithAll :: Entity -> [Entity] -> Entity
--each entity is checked against all others, at each step. Result contains 'Maybes', which we extract elsewhere
collideWithAll entity = foldr f v
  where
    v = entity
    f x a = case a of
              (Entity Destruction _ _ _ _ _ _)  -> a                    -- if its already destroyed you dont need to handle collision anymore
              _                               -> if checkCollision a x then handleCollision a x else a

--changes this entity's health, or returns 'Nothing' if health is expired or has none
handleCollision :: Entity -> Entity -> Entity
handleCollision e1@(Entity (Shooter health _) _ _ _ _ _ _) e2@(Entity (Shooter _ _) _ _ _ _ _ _)      -- Two shooters destroy eachother
  = destroyEntity e1
handleCollision e1@(Entity (Shooter health incs) fac loc hbox speed angle mID) e2@(Entity (Bullet damage) _ _ _ _ _ _)  -- Bullet reduces health of shooter
  = if health - damage <= 0 then destroyEntity e1 else Entity (Shooter (health - damage) incs) fac loc hbox speed angle mID
handleCollision e1@(Entity (Bullet damage1) fac loc hbox speed angle mID) e2@(Entity (Bullet damage2) _ _ _ _ _ _)   -- Stronger bullet survives, otherwise both are destroyed
  = destroyEntity e1--if damage1 > damage2 then e1 else destroyEntity e1
handleCollision e1@(Entity (Bullet damage1) fac loc hbox speed angle mID) e2@(Entity (Shooter _ _) _ _ _ _ _ _)      -- Bullet is removed when it hits a Player
  = destroyEntity e1
handleCollision e1 _ = e1

destroyEntity :: Entity -> Entity
destroyEntity (Entity _ _ loc hbox _ _ _) = Entity Destruction Neutral loc hbox 0 0 (-2)

isDestroyed :: Entity -> Bool
isDestroyed (Entity Destruction _ _ _ _ _ _) = True
isDestroyed _ = False

-- This is very simplistic at the moment, but as long as entities are just squares, it will work fine
checkCollision :: Entity -> Entity -> Bool
checkCollision e1@(Entity type1 fac1 loc1 (w1,h1) speed1 angle1 mID1) e2@(Entity type2 fac2 loc2 (w2,h2) speed2 angle2 mID2)
  | fac1 == fac2 = False
  | otherwise  = (checkDistance loc1 loc2 <= max (w1+w2) (h1+h2))  --if not the same faction and hit =True
  
checkDistance :: Location -> Location -> Float
checkDistance (x1,y1) (x2,y2) = sqrt( (x2-x1)**2 + (y2-y1)**2)



--(Entity type fac loc hbox speed angle)

findEntity :: Entity -> Location
findEntity (Entity _ _ loc _ _ _ _) = loc




-- Debugging functions--
getEntityType :: Entity -> EntityType Float 
getEntityType (Entity etype _ _ _ _ _ _) = etype

isOfEType :: EntityType Float -> Entity -> Bool
isOfEType (Shooter _ _) (Entity etype2 _ _ _ _ _ _) = case etype2 of
  Shooter _ _ -> True
  _ -> False
isOfEType (Bullet _) (Entity etype2 _ _ _ _ _ _) = case etype2 of
  Bullet _ -> True
  _ -> False
isOfEType (Obstacle _) (Entity etype2 _ _ _ _ _ _) = case etype2 of
  Obstacle _ -> True
  _ -> False
isOfEType (Destruction) (Entity etype2 _ _ _ _ _ _) = case etype2 of
  Destruction -> True
  _ -> False

getPlayerStatus :: Entity -> String
getPlayerStatus (Entity (Shooter h _) Player _ _ _ _ _) = "Health: " ++ show h
getPlayerStatus (Entity (Shooter h _) Enemy _ _ _ _ _) = "is enemy"
getPlayerStatus (Entity (Bullet _) Player _ _ _ _ _) = "Is friendly bullet"
getPlayerStatus (Entity (Bullet _) Enemy _ _ _ _ _) = "Is enemy bullet"
getPlayerStatus (Entity (Destruction) _ _ _ _ _ _) = "Dead"
getPlayerStatus _ = "error?"










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


