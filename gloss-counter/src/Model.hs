{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use list literal" #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where

data MainState = MGame GameState | MMenu MenuState

data GameState = GameState {  world :: World
                            , score :: Int
                            , elapsedTime :: Float
                            , gameKeys :: KeysOfInput
                           }

data MenuState = MenuState {  menu :: Menu
                            , game :: GameState
                            , menuKeys :: KeysOfInput
                            , inputDelay :: Float
                           }

data Menu = Menu {  options :: [MenuOption]
                  , selectPointer :: Int
                 }

data MenuOption = MenuOption {  optionText :: String
                              , index :: Int
                              , nextState :: MainState
                             }

data World = World  { player :: Entity
                    , entities :: [Entity]
                    , scrollSpeed :: Float
                    , spawnIncrement :: Float
                    , overlay :: String          -- for debugging during the game, at the moment.
                    -- , background :: Picture   -- variabele die verwijst naar een picture
                    }

data KeysOfInput = Keys { keyPressedW :: Bool
                        , keyPressedA :: Bool
                        , keyPressedS :: Bool
                        , keyPressedD :: Bool
                        , keyPressedP :: Bool
                        , keyPressedSpace :: Bool
                        }

data Entity = Entity  { eType :: EntityType Float
                      , faction :: Faction
                      , hitbox :: Hitbox
                      , movement :: Movement
                      -- , sprite :: Picture
                      }
  --deriving (Eq a)

data Movement = Movement {
                          location :: Location,
                          speed :: Float, 
                          angle :: Float, 
                          movementPaternID :: Int, 
                          moveWithWorld :: Bool
                         }
data EntityType a = Shooter a (Float,Float) | Bullet a | Obstacle a | Destruction
--                          |      |  |              |            |
--                          V      V  V              V            V
--                      health   (increments)     damage        damage/health?

data Faction = Player | Enemy | Neutral
  deriving Eq

type Location = (Float, Float)

type Hitbox = (Float, Float)


--  INITIALISED DATA --
initialState :: MainState
initialState = MMenu mainMenu
-- initialState = MGame (GameState newWorld 0 0 (Keys False False False False False))

emptyKeys :: KeysOfInput
emptyKeys = (Keys False False False False False False)

menuInputDelay :: Float
menuInputDelay = 0.5

-- INITIALISED MENUS --
mainMenu :: MenuState
mainMenu = MenuState  ( Menu
                        ( (MenuOption "New Game" 1 (MGame newGame)) 
                        : (MenuOption "Continue Game" 2 (MGame newGame))
                        : (MenuOption "Quit Game" 3 (MGame newGame))
                        : [])
                        1    -- Index starts at 1
                      ) 
                      emptyGame
                      emptyKeys
                      menuInputDelay

pauseMenu :: MenuState
pauseMenu = MenuState  ( Menu
                        ( (MenuOption "Continue Game" 1 (MGame emptyGame)) 
                        : (MenuOption "Reset Game" 2 (MGame newGame))
                        : (MenuOption "Main Menu" 3 (MMenu mainMenu))
                        : [])
                        1    -- Index starts at 1
                      ) 
                      emptyGame
                      emptyKeys
                      menuInputDelay

-- INITIALISED GAMESTATES/WORLDS --
newGame :: GameState
newGame = GameState newWorld 0 0 emptyKeys

emptyGame :: GameState
emptyGame = GameState emptyWorld 0 0 emptyKeys

emptyWorld :: World
emptyWorld = World emptyEntity [] 0 9999 ""

newWorld :: World
newWorld = World playerEntity [] initialScrollSpeed initialSpawnIncrement ""


-- INITIALISED ENTITIES --
emptyEntity :: Entity
emptyEntity = Entity (Obstacle 1) Neutral (0,0) (Movement (0,0) 0 0 (-2) False)

playerEntity :: Entity
playerEntity = Entity (Shooter 10 ((0.5),0)) Player (30,30) (Movement (20, negate (y/2))  10 0 0 False)
  where y = fromIntegral screenHeight :: Float


enemyTypes :: [Entity]
enemyTypes =    [staticEnemy, aimingEnemy]

staticEnemy :: Entity
staticEnemy = Entity (Shooter 30 (1.5,0)) Enemy (x, y/2) (10,10) 0 0 1
  where
    x = fromIntegral screenWidth :: Float
    y = fromIntegral screenHeight :: Float

aimingEnemy :: Entity
aimingEnemy = Entity (Shooter 30 (3,0)) Enemy (x, y/2) (10,10) (negate initialScrollSpeed) 0 2
  where
    x = fromIntegral screenWidth :: Float
    y = fromIntegral screenHeight :: Float






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
