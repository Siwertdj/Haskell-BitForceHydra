-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

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
        keys)


updateWorld :: World -> Float -> KeysOfInput -> IO World
updateWorld (World player entities speed spawnIncrement playerShotInc) time keys =
    do
      newEnemy <- spawnEnemy
      return $ World
                (handlePlayer player keys)
                ( ([Entity (Bullet 1) Player (findEntity player) (1,1) (-10) (0,0) | spawnShootIncrement player (playerShotInc - time) keys]) ++
                  --([Entity (Bullet 1) Enemy (findEntity enemy) (1,1) (-10) (0,0) | enemy@(Entity _ fac _ _ _ _) <- move, fac == Enemy && spawnShootIncrement enemy (playerShotInc - time) keys]) ++
                  (if spawnShootIncrement newEnemy (spawnIncrement - time) keys then newEnemy : move else move)
                      
                )
                speed
                (if time > spawnIncrement then time + spawnIncrement else spawnIncrement)     -- next spawn time is upped by 5 seconds (the increment) when the elapsedTime passes its current value, thus every 5 seconds something will spawn.
                (if isShooting keys (playerShotInc - time) then time + playerShotInc else playerShotInc)
      where
        move = movingEntities entities speed

                      --([Entity (Bullet 1) Player (findEntity player) (1,1) (-10) (0,0) | spawnShootIncrement player (playerShotInc - time) keys]) ++
                      --(if spawnShootIncrement newEnemy (spawnIncrement - time) keys then newEnemy : move else move) ++
                      --([Entity (Bullet 1) Enemy (findEntity player) (1,1) (-10) (0,0) | spawnShootIncrement player (playerShotInc - time) keys])

isShooting :: KeysOfInput -> Float ->  Bool
isShooting (Keys _ _ _ _ spaceIn) time = spaceIn && time <= 0

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




movingPlayer :: Location -> Hitbox -> KeysOfInput -> Location
movingPlayer (locX,locY) (width, height) (Keys wIn aIn sIn dIn _)
  | wIn && not sIn && locY < (upperBound-height/2) = (locX, locY + playerSpeed)
  | sIn && not wIn && locY > (lowerBound+height/2) = (locX, locY - playerSpeed)
  | aIn && not dIn && locX > (leftBound+width/2) = (locX - playerSpeed, locY)
  | dIn && not aIn && locX < (rightBound-width/2) = (locX + playerSpeed, locY)
  | otherwise = (locX, locY)

movingEntities :: [Entity] -> Float -> [Entity]
movingEntities entities scrollSpeed = map (move scrollSpeed) entities

move :: Float -> Entity -> Entity
move scrollSpeed entity@(Entity eType faction location hitbox speed angle)
  = (entity \/ scrollSpeed) \/ speed

spawnShootIncrement :: Entity -> Float -> KeysOfInput -> Bool
spawnShootIncrement e@(Entity _ fac _ _ _ _) time keys = case fac of
                                                  Player -> isShooting keys time
                                                  Enemy -> time <= 0



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

findEntity :: Entity -> Location
findEntity (Entity _ _ loc _ _ _) = loc

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


