{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# language NamedFieldPuns #-}
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
import System.Exit (exitSuccess)
import GHC.IO.IOMode (IOMode(ReadWriteMode))
import GHC.IO.Handle (hGetContents, hPutStr, hClose)
import Control.Monad
 
 
-- | Handle one iteration of the game
step :: Float -> MainState -> IO MainState
step secs mstate@(MMenu menu@(MenuState mNav@(Menu options index) gameState keys))                                   --  IS IN MENU
  = if keyPressedSpace $ menuKeys menu
      then  return $ confirmMenuOption $ options !!  (index-1)
      else  return $ MMenu (MenuState
              (navigateMenu mNav keys (True))
              gameState
              (keys {keyPressedW = False, keyPressedS = False})   -- resets input to prevent 'holding them down' and cycling the menu too quickly
              )
step secs mstate@(MGame gstate@(GameState world score elapsedTime keys@(Keys _ _ _ _ pIn _)))                  --  IS IN GAME
  | getPlayerStatus (player world) == "Dead" = 
    do
      saveRecord elapsedTime score
      return $ MMenu gameOverMenu
  | otherwise = if pIn -- If wanting to pause, return the pause-menu
      then return $ MMenu $ 
        (storeGameState 
          (pauseMenu {menu = (changeMenuOption (getMenu pauseMenu)  1 (setOptionNextState) -- Currently there is a minor bug which hides the 'Continue game' option from the pause-menu
                              (MGame gstate {gameKeys = (keys {keyPressedP = False})}) )}) 
          gstate
        )          
      else do
        updateWorld' <- updateWorld world elapsedTime keys
        let -- Retrieve world and new score from the updateWorld function
          newWorld = fst $ updateWorld'
          accumulatedScore = snd $ updateWorld'
        return $ MGame (GameState
          newWorld
          (score + accumulatedScore)
          (elapsedTime + secs)
          keys
          )
step _ Exit   -- close game
  = exitSuccess


{-__  __                     __                  _   _                 
 |  \/  |                   / _|                | | (_)                
 | \  / | ___ _ __  _   _  | |_ _   _ _ __   ___| |_ _  ___  _ __  ___ 
 | |\/| |/ _ \ '_ \| | | | |  _| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
 | |  | |  __/ | | | |_| | | | | |_| | | | | (__| |_| | (_) | | | \__ \
 |_|  |_|\___|_| |_|\__,_| |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|___/-}

navigateMenu :: Menu -> KeysOfInput -> Bool -> Menu
navigateMenu (Menu ops index) (Keys wIn _ sIn _ _ spaceIn) able | able && wIn && (not sIn)  = (Menu ops (cycleNavigation $ index-1))
                                                              | able && sIn && not wIn    = (Menu ops (cycleNavigation $ index+1))
                                                              | otherwise = (Menu ops index)
                                                                  where 
                                                                    navigationLength = length ops
                                                                    cycleNavigation :: Int -> Int
                                                                    cycleNavigation n | n < 1 = navigationLength
                                                                                      | n > navigationLength = 1
                                                                                      | otherwise = n

confirmMenuOption :: MenuOption -> MainState
confirmMenuOption (MenuOption _ _ next) = next

getMenu :: MenuState -> Menu
getMenu (MenuState menu _ _) = menu

changeMenuOption :: Menu -> Int -> (MenuOption -> MainState -> MenuOption) -> MainState -> Menu
changeMenuOption (Menu options i) index f newState = Menu (addInbetween  (f (options !! index) newState)) i
  where 
    splitList options = splitAt index options
    addInbetween a = (take (index-1) (fst $ splitList options)) ++  [a] ++ (snd $ splitList options)
  
setOptionNextState :: MenuOption -> MainState -> MenuOption
setOptionNextState option mainState = (option {nextState = mainState})
--setOptionNextState (MenuOption text index nextState) mainState = (MenuOption text index mainState)

storeGameState :: MenuState -> GameState -> MenuState
storeGameState menuState currentGame = (menuState {game = currentGame})




{- _____                         __                  _   _                 
  / ____|                       / _|                | | (_)                
 | |  __  __ _ _ __ ___   ___  | |_ _   _ _ __   ___| |_ _  ___  _ __  ___ 
 | | |_ |/ _` | '_ ` _ \ / _ \ |  _| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
 | |__| | (_| | | | | | |  __/ | | | |_| | | | | (__| |_| | (_) | | | \__ \
  \_____|\__,_|_| |_| |_|\___| |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|___/-}                                                                        

updateWorld :: World -> Float -> KeysOfInput -> IO(World,Float)
updateWorld w@(World player entities speed spawnIncrement overlay) time keys =
    do    -- generate a new, random enemy
      newEnemy <- spawnEnemy
      let -- seperate destroyed entities from living entities
        destroyedEntities = filter (isDestroyed) entities
        filteredEntities = filter (not. isOutOfBounds) $ filter (not.isDestroyed) entities
      let -- handle shooting and movement
        canShoot' entity' = shootTimer entity' time keys
        entitiesCanShoot = map canShoot' (movingEntities filteredEntities speed)
        updatedEntities = map fst entitiesCanShoot

      return $ (World
                ( -- Update Player --
                  collideWithAll 
                  (handlePlayer (fst $ canShoot' player) keys) 
                  filteredEntities
                )
                ( -- Update Entities --
                   map (allignToPlayer player) $ collision (
                  (if (spawnIncrement - time) <= 0 then map (changeAngle player) (newEnemy : updatedEntities) else map (changeAngle player) updatedEntities)
                  ++
                  ([Entity (Bullet 30) Player (10,10) (Movement (findEntity player) (-10) 0 (0,0) (-1) False) | snd (canShoot' player)])
                  ++
                  ([changeDirection player (Entity (Bullet 1) fac (10,10) (Movement eLoc 10 angle (0,0) (-1) mWW))| (enemy@(Entity _ fac _ (Movement eLoc _ angle _ mpID mWW)), shooting) <- entitiesCanShoot, shooting && fac == Enemy])
                  )
                )
                (-- Update scrollspeed --
                  newScrollSpeed
                )
                ( -- Update spawnIncrement -- 
                  if time > spawnIncrement then time + newSpawnIncrement else spawnIncrement -- next spawn time is upped by 5 seconds (the increment) when the elapsedTime passes its current value, thus every 5 seconds something will spawn.
                )
                ( -- UN-COMMENT STRINGS FOR DEBUGGING --
                  ( --"Number of shooters: " ++ show (length (filter (isOfEType (Shooter 0 (0,0))) entities)) ++ "   " ++ 
                    --"Number of bullets: " ++ show (length (filter (isOfEType (Bullet 0)) entities)) ++ "   " ++ 
                    --"Scrollspeed: " ++ (show(newScrollSpeed)) ++ "   " ++
                    "Spawn every " ++ (show (newSpawnIncrement)) ++ " seconds" ++ "   " ++
                    "Status: " ++ (show (getPlayerStatus player)) ++ "   " ++ 
                    --"Player loc: " ++ (show $ fst (findEntity player)) ++ ", " ++ (show $ snd (findEntity player)) ++ "   " ++
                    --"New score: " ++ (show $ fromIntegral $ length (filter (isOfEType (Shooter 0 (0,0))) destroyedEntities)) ++ "   " ++
                    "Time: " ++ (show time)
                  )
                )
                ,  -- the accumulated score for this cycle - how many enemies were destroyed?
                (fromIntegral $ length (filter (wasOfETypeAndFac (Shooter 0 (0,0)) Enemy) destroyedEntities)))     
      where
        newSpawnIncrement = scaleOverTime time initialSpawnIncrement False
        newScrollSpeed = scaleOverTime time initialScrollSpeed True

scaleOverTime :: Float -> Float -> Bool -> Float
scaleOverTime time value scaleUp = if scaleUp then value * modifierUp else value * modifierDown
  where 
    modifierUp = 1 + (0.05) * (roundDown(time / scaleInterval))
    modifierDown = 1 / (1 + (roundDown(time / scaleInterval))*(0.05))

roundDown :: Float -> Float
roundDown n = fromIntegral $ floor n

spawnEnemy :: IO Entity
spawnEnemy =
    do
      let amountOfEnemyTypes = length enemyTypes -1
      randomLocation  <- randomRIO(leftBound, rightBound) :: IO Float
      randomSelector  <- randomRIO(0, amountOfEnemyTypes) :: IO Int
      let randomEnemy = enemyTypes !! randomSelector
      return $ randomEnemy {movement = (findEntity' randomEnemy) {location = (randomLocation, upperBound)}} -- {location = (randomLocation, upperBound)}


handlePlayer :: Entity -> KeysOfInput -> Entity
handlePlayer player@(Entity etype faction hitbox (Movement loc speed angle dir mID moveWWorld)) keys
  = Entity
      etype
      faction
      hitbox --(movingPlayer loc hitbox keys)         -- change location
      (Movement (movingPlayer loc hitbox keys) speed angle dir mID moveWWorld)
      



-- HANDLE MOVING --
movingPlayer :: Location -> Hitbox -> KeysOfInput -> Location
movingPlayer (locX,locY) (width, height) (Keys wIn aIn sIn dIn _ _)
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
moveEntities scrollSpeed entity@(Entity (Bullet _) _ _ _) = moveBullet scrollSpeed entity
moveEntities scrollSpeed entity@(Entity eType faction hitbox (Movement location speed angle dir mID moveWithWorld))
  = (entity \/ scrollSpeed) \/ speed

moveBullet :: Float -> Entity -> Entity
moveBullet scroll e@(Entity (Bullet hp) Enemy _ (Movement (ex,ey) spd angle (dx,dy) (-1) True)) 
  = e /\ (spd * dy) ~> (spd * dx)
moveBullet scroll e@(Entity (Bullet hp) Enemy _ (Movement (ex,ey) spd angle (dx,dy) mpid False)) 
  = e \/ scroll \/ spd
moveBullet scroll e@(Entity (Bullet hp) Player _ (Movement (ex,ey) spd angle (dx,dy) mpid moveWithWorld)) 
  = e \/ scroll \/ spd
moveBullet scroll e@(Entity _ _ _ _) 
  = e

changeAngle :: Entity -> Entity -> Entity
changeAngle p@(Entity _ Player _ ( Movement (px,py)  _ _ _ _ _)) e@(Entity eType Enemy hb (Movement (ex,ey) spd angle dir 1 moveWithWorld)) =
  Entity eType Enemy hb (Movement (ex,ey) spd newAngle (distX,distY) 1 moveWithWorld)
 where distX = px - ex
       distY = py - ey
       newAngle = radToDeg nAngle
       nAngle = normalizeAngle (atan (distX/distY))
changeAngle p e = e

changeDirection :: Entity -> Entity -> Entity
changeDirection p@(Entity _ Player _ (Movement (px,py) _ _ _ _ _)) e@(Entity eType Enemy hb (Movement (ex,ey) spd angle (dx,dy) mpID moveWithWorld)) 
   = Entity eType Enemy hb  (Movement (ex,ey) spd angle (normalize (distX,distY)) mpID moveWithWorld)
      where distX = px - ex
            distY = py - ey
changeDirection _ e = e

normalize :: (Float,Float) -> (Float,Float)
normalize (x,y) = (x / unit, y / unit)
  where unit = sqrt (x*x + y*y)

allignToPlayer :: Entity -> Entity -> Entity
allignToPlayer p@(Entity _ Player _ (Movement (px,py) _ _ _ _ _)) e@(Entity eType Enemy hb (Movement (ex,ey) spd angle dir 2 moveWithWorld)) 
  | px > ex - spd = Entity eType Enemy hb (Movement (ex - spd,ey) spd angle dir 2 moveWithWorld)
  | px < ex + spd = Entity eType Enemy hb (Movement (ex + spd,ey) spd angle dir 2 moveWithWorld)
  | otherwise = e
allignToPlayer p e = e

-- If out of bounds == True
isOutOfBounds :: Entity -> Bool
isOutOfBounds (Entity _ _ _ (Movement (x,y) _ _ _ _ _)) = y < lowerBound || y > upperBound || x < leftBound || x > rightBound


-- HANDLE SHOOTING --
isShooting :: KeysOfInput -> Bool
isShooting (Keys _ _ _ _ _ spaceIn) = spaceIn

canShoot :: Entity -> Float -> Bool
canShoot e@(Entity (Shooter _ (inc, next)) fac _ _) time   = case fac of
                                                  Player  -> next - time <= 0
                                                  Enemy   -> next - time <= 0
                                                  _       -> False              -- Neutrals
canShoot _ _ = False

-- The return type of (Entity, Bool) represents (<Entity in question>, <Can it shoot?>)
shootTimer :: Entity -> Float -> KeysOfInput -> (Entity, Bool)
shootTimer e@(Entity (Shooter hp (inc, next)) Player hb (Movement loc spd ang dir mID moveWithWorld)) time keys =
  if canShoot e time && isShooting keys
    then (Entity (Shooter hp (inc, time + inc)) Player hb (Movement loc spd ang dir mID moveWithWorld), True)
    else (e,False)
shootTimer e@(Entity (Shooter hp (inc, next)) Enemy hb (Movement loc spd ang dir mID moveWithWorld)) time _ =
  if canShoot e time
    then (Entity (Shooter hp (inc, time + inc)) Enemy hb (Movement loc spd ang dir mID moveWithWorld), True)
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
              (Entity (Destruction _ _) _ _ _)  -> a                    -- if its already destroyed you dont need to handle collision anymore
              _                               -> if checkCollision a x then handleCollision a x else a

--changes this entity's health, or returns 'Nothing' if health is expired or has none
handleCollision :: Entity -> Entity -> Entity
handleCollision e1@(Entity (Shooter health _) _ _ _) e2@(Entity (Shooter _ _) _ _ _)      -- Two shooters destroy eachother
  = destroyEntity e1
handleCollision e1@(Entity (Shooter health incs) fac hbox (Movement loc speed angle dir mID moveWithWorld)) e2@(Entity (Bullet damage) _ _ _)  -- Bullet reduces health of shooter
  = if health - damage <= 0 then destroyEntity e1 else Entity (Shooter (health - damage) incs) fac hbox (Movement loc speed angle dir mID moveWithWorld)
handleCollision e1@(Entity (Bullet damage1) fac hbox (Movement loc speed angle dir mID moveWithWorld)) e2@(Entity (Bullet damage2) _ _ _)   -- Stronger bullet survives, otherwise both are destroyed
  = destroyEntity e1--if damage1 > damage2 then e1 else destroyEntity e1
handleCollision e1@(Entity (Bullet damage1) fac hbox (Movement loc speed angle dir mID moveWithWorld)) e2@(Entity (Shooter _ _) _ _ _)      -- Bullet is removed when it hits a Player
  = destroyEntity e1
handleCollision e1 _ = e1

destroyEntity :: Entity -> Entity
destroyEntity (Entity etype fac hbox (Movement loc _ _ _ _ _)) = Entity (Destruction etype fac) Neutral hbox (Movement loc 0 0 (0,0) (-2) False)

isDestroyed :: Entity -> Bool
isDestroyed (Entity (Destruction _ _) _ _ _) = True
isDestroyed _ = False

-- This is very simplistic at the moment, but as long as entities are just squares, it will work fine
checkCollision :: Entity -> Entity -> Bool
checkCollision e1@(Entity type1 fac1 (w1,h1) (Movement loc1 speed1 angle1 dir1 mID1 moveWithWorld1)) e2@(Entity type2 fac2 (w2,h2) (Movement loc2 speed2 angle2 dir2 mID2 moveWithWorld2))
  | fac1 == fac2 = False
  | otherwise  = (checkDistance loc1 loc2 <= max (w1+w2) (h1+h2))  --if not the same faction and hit =True
  
checkDistance :: Location -> Location -> Float
checkDistance (x1,y1) (x2,y2) = sqrt( (x2-x1)**2 + (y2-y1)**2)



--(Entity type fac loc hbox speed angle)

findEntity :: Entity -> Location
findEntity (Entity _ _ _ (Movement loc _ _ _ _ _)) = loc


findEntity' :: Entity -> Movement
findEntity' (Entity _ _ _ movement) = movement

-- Debugging functions--
getEntityType :: Entity -> EntityType Float 
getEntityType (Entity etype _ _ _) = etype


isOfEType :: EntityType Float -> Entity -> Bool
isOfEType (Shooter _ _) (Entity etype2 _ _ _) = case etype2 of
  Shooter _ _ -> True
  _ -> False
isOfEType (Bullet _) (Entity etype2 _ _ _) = case etype2 of
  Bullet _ -> True
  _ -> False
isOfEType (Obstacle _) (Entity etype2 _ _ _) = case etype2 of
  Obstacle _ -> True
  _ -> False
isOfEType (Destruction _ _) (Entity etype2 _ _ _) = case etype2 of
  (Destruction _ _) -> True
  _ -> False

wasOfETypeAndFac :: EntityType Float -> Faction -> Entity -> Bool
wasOfETypeAndFac etype1 fac1 (Entity (Destruction etype2 fac2) _ _ _) = (isOfEType etype1 (emptyEntity {eType = etype2})) && fac1 == fac2
wasOfETypeAndFac _ _ _ = False

getPlayerStatus :: Entity -> String
getPlayerStatus (Entity (Shooter h _) Player _ _) = "Health: " ++ show h
getPlayerStatus (Entity (Shooter h _) Enemy _ _) = "is enemy"
getPlayerStatus (Entity (Bullet _) Player _ _) = "Is friendly bullet"
getPlayerStatus (Entity (Bullet _) Enemy _ _) = "Is enemy bullet"
getPlayerStatus (Entity (Destruction _ _) _ _ _) = "Dead"
getPlayerStatus _ = "error?"



-- FILE WRITING --
saveRecord :: Float -> Float -> IO()
saveRecord time score = do
  scores <- readFile "highscores.txt"
  let newScores = scores ++ "\n" ++  writeRecord time score
  when (length scores > 0) $
    writeFile "highscores.txt" newScores


writeRecord :: Float -> Float -> String
writeRecord time score =     "Score:"
                          ++ scoreString  ++ replicate (scoreLength - length scoreString) ' ' 
                          ++ "| Time:"
                          ++ timeString ++ replicate (timeLength - length timeString) ' '
  where
    timeString = show time
    scoreString = show score
    timeLength = 10 :: Int
    scoreLength = 5 :: Int





class Move a where      -- (Float, Float) 
  (/\) , (\/), (~>), (<~) :: a -> Float -> a          -- /\ is up, \/ is down

instance Move Entity where
  entity /\ y = entity { movement = (findEntity' entity) {location = (fst $ findEntity entity, snd (findEntity entity) + y) }}
  entity \/ y = entity { movement = (findEntity' entity) {location = (fst $ findEntity entity, snd (findEntity entity) - y) }}
  entity ~> x = entity { movement = (findEntity' entity) {location = (fst (findEntity entity) + x, snd $ findEntity entity) }}
  entity <~ x = entity { movement = (findEntity' entity) {location = (fst (findEntity entity) - x, snd $ findEntity entity) }}













{-_____                   _      __                  _   _                 
 |_   _|                 | |    / _|                | | (_)                
   | |  _ __  _ __  _   _| |_  | |_ _   _ _ __   ___| |_ _  ___  _ __  ___ 
   | | | '_ \| '_ \| | | | __| |  _| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
  _| |_| | | | |_) | |_| | |_  | | | |_| | | | | (__| |_| | (_) | | | \__ \
 |_____|_| |_| .__/ \__,_|\__| |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|___/
             | |                                                           
             |_|                                                           -}

input :: Event -> MainState -> IO MainState
input e mstate = return (inputKey e mstate)

inputKey :: Event -> MainState -> MainState
-- GAME INPUT --
    --moving input
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
    -- Misc input
inputKey (EventKey (Char 'p') Down _ _) mstate@(MGame (GameState w s t keys))
  = MGame (GameState w s t (keys {keyPressedP = True}))
inputKey (EventKey (Char 'p') Up _ _) mstate@(MGame (GameState w s t keys))
  = MGame (GameState w s t (keys {keyPressedP = False}))

-- MENU INPUT --
  --navigational
inputKey (EventKey (Char 'w') Down _ _) mstate@(MMenu (MenuState (Menu options index) game keys))
  = (MMenu (MenuState (Menu options index) game (keys {keyPressedW = True})))
inputKey (EventKey (Char 'w') Up _ _) mstate@(MMenu (MenuState (Menu options index) game keys))
  = (MMenu (MenuState (Menu options index) game (keys {keyPressedW = False})))
inputKey (EventKey (Char 's') Down _ _) mstate@(MMenu (MenuState (Menu options index) game keys))
  = (MMenu (MenuState (Menu options index) game (keys {keyPressedS = True})))
inputKey (EventKey (Char 's') Up _ _) mstate@(MMenu (MenuState (Menu options index) game keys))
  = (MMenu (MenuState (Menu options index) game (keys {keyPressedS = False})))
  --confirm selection
inputKey (EventKey (SpecialKey KeySpace) Down _ _) mstate@(MMenu (MenuState (Menu options index) game keys))
  = (MMenu (MenuState (Menu options index) game (keys {keyPressedSpace = True})))
inputKey (EventKey (SpecialKey KeySpace) Up _ _) mstate@(MMenu (MenuState (Menu options index) game keys))
  = (MMenu (MenuState (Menu options index) game (keys {keyPressedSpace = False})))

-- Lack of input
inputKey _ mstate = mstate -- Otherwise keep the same


