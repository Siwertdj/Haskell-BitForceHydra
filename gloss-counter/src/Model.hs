-- | This module contains the data types
--   which represent the state of the game
module Model where

data MainState = MGame GameState | MMenu MenuState

data GameState = GameState {  world :: World
                            , score :: Int
                            , elapsedTime :: Float
                            , keys :: KeysOfInput
                           }

data MenuState = MenuState {  menu :: Menu
                            , game :: GameState
                           }

data Menu = Menu {  options :: [String]
                 -- , selectPointer
                 }

data World = World  { player :: Entity
                    , entities :: [Entity]
                    , scrollSpeed :: Float
                    , spawnIncrement :: Float
                    -- , background :: Picture   -- variabele die verwijst naar een picture
                    }

data KeysOfInput = Keys { keyPressedW :: Bool
                        , keyPressedA :: Bool
                        , keyPressedS :: Bool
                        , keyPressedD :: Bool
                        , keyPressedSpace :: Bool
                        }

data Entity = Entity  { eType :: EntityType Float
                      , faction :: Faction
                      , location :: Location
                      , hitbox :: Hitbox
                      , speed :: Float
                      , angle :: (Float, Float)
                      -- , sprite :: Picture
                      }
  --deriving (Eq a)

data EntityType a = Shooter a (Float,Float) | Bullet a | Obstacle a | Destruction
--                          |      |  |              |            |
--                          V      V  V              V            V
--                      health   (increments)     damage        damage/health?

{-
instance Eq a => Eq Entity where
  e1@(Entity eType1 fac1 loc1 hbox1 speed1 angle1) == e2@(Entity eType2 fac2 loc2 hbox2 speed2 angle2) =
    eType1 == eType2 &&
    fac1 == fac2 &&
    loc1 == loc2 &&
    hbox1 == hbox2 &&
    speed1 == speed2 &&
    angle1 == angle2
-}


data Faction = Player | Enemy | Neutral
  deriving (Eq)

type Location = (Float, Float)

type Hitbox = (Float, Float)


--  INITIALISED DATA --

initialState :: MainState
-- initialState = MMenu ( MenuState (menu-type) (game-type) )
initialState = MGame (GameState newWorld 0 0 (Keys False False False False False))

emptyGame :: GameState
emptyGame = GameState emptyWorld 0 0 (Keys False False False False False )

emptyWorld :: World
emptyWorld = World emptyEntity [] 0 9999 

emptyEntity :: Entity
emptyEntity = Entity (Obstacle 1) Neutral (0, 0) (0,0) 0 (0,0)


newWorld :: World
newWorld = World playerEntity [] initialScrollSpeed initialSpawnIncrement 

playerEntity :: Entity
playerEntity = Entity (Shooter 10 ((0.5),0)) Player (20, negate (y/2)) (30,30) 10 (0,0)
  where y = fromIntegral screenHeight :: Float

staticEnemy :: Entity
staticEnemy = Entity (Shooter 30 (1.5,0)) Enemy (x, y/2) (20,20) 0 (0,0)
  where
    x = fromIntegral screenWidth :: Float
    y = fromIntegral screenHeight :: Float

aimingEnemy :: Entity
aimingEnemy = Entity (Shooter 30 (3,0)) Enemy (x, y/2) (30,30) (negate initialScrollSpeed) (0,0)
  where
    x = fromIntegral screenWidth :: Float
    y = fromIntegral screenHeight :: Float

enemyTypes :: [Entity]
enemyTypes =    [staticEnemy, aimingEnemy]


-- VARIABLES --
playerSpeed :: Float
playerSpeed = 5

initialScrollSpeed :: Float
initialScrollSpeed = 2

initialSpawnIncrement :: Float
initialSpawnIncrement = 5

playerShootIncrement :: Float
playerShootIncrement = 2

--initializes the screen parameters. We might be able to fetch these based on the system later
screenWidth, screenHeight :: Int
screenWidth = 1024
screenHeight = 576

upperBound, lowerBound, leftBound, rightBound :: Float
upperBound = y / 2
  where y = fromIntegral screenHeight :: Float
lowerBound = negate (y/2)
  where y = fromIntegral screenHeight :: Float
leftBound = negate (x/2)
  where x = fromIntegral screenWidth :: Float
rightBound = x / 2
  where x = fromIntegral screenWidth :: Float
