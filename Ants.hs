module Ants
  (
    -- Data structures
    Owner (..)
  , Ant (..)
  , Direction (..)
  , GameParams (..)
  , GameState (..)
  , Order (..)
  , World

    -- Utility functions
  , myAnts -- return list of my Ants
  , enemyAnts -- return list of visible enemy Ants
  , passable
  , distance
  , timeRemaining

    -- main function
  , game
  ) where

import Control.Applicative
import Control.Monad (when)

import Data.Array
import Data.Array.IO
import Data.List (isPrefixOf)
import Data.Char (digitToInt, toUpper)
import Data.Maybe (fromJust)

import Data.Time.Clock
import System.IO

import Util (fOr, tuplify2)

timeRemaining :: GameState -> IO NominalDiffTime
timeRemaining gs = do
  timeNow <- getCurrentTime
  return $ timeNow `diffUTCTime` startTime gs

--------------------------------------------------------------------------------
-- Points ----------------------------------------------------------------------
--------------------------------------------------------------------------------
type Row = Int
type Col = Int
type Point = (Row, Col)

row :: Point -> Row
row = fst

col :: Point -> Col
col = snd

--------------------------------------------------------------------------------
-- Tiles -----------------------------------------------------------------------
--------------------------------------------------------------------------------
data Tile = AntTile Owner
          | Dead Owner
          | Land
          | FoodTile
          | Water
          | Unknown
          deriving (Show,Eq)

type Visible = Bool

-- | Elements of the world
data MetaTile = MetaTile
  { tile :: Tile
  , visible :: Visible
  } deriving (Show)

isAnt, isDead :: Tile -> Bool
isAnt (AntTile _) = True
isAnt _ = False

isDead (Dead _) = True
isDead _ = False

-- | For debugging
renderTile :: MetaTile -> String
renderTile m
  | tile m == AntTile Me = visibleUpper m 'm'
  | tile m == AntTile Enemy1 = visibleUpper m 'a'
  | tile m == AntTile Enemy2 = visibleUpper m 'b'
  | tile m == AntTile Enemy3 = visibleUpper m 'c'
  | tile m == Dead Me = visibleUpper m 'd'
  | tile m == Dead Enemy1 = visibleUpper m 'd'
  | tile m == Dead Enemy2 = visibleUpper m 'd'
  | tile m == Dead Enemy3 = visibleUpper m 'd'
  | tile m == Land = visibleUpper m 'l'
  | tile m == FoodTile = visibleUpper m 'f'
  | tile m == Water = visibleUpper m 'w'
  | otherwise = "*"
  where
    visibleUpper :: MetaTile -> Char -> String
    visibleUpper mt c
      | visible mt = [toUpper c]
      | otherwise  = [c]

-- | Sets the tile to visible, if the tile is still unknown then it is land.
visibleMetaTile :: MetaTile -> MetaTile
visibleMetaTile m
  | tile m == Unknown = MetaTile {tile = Land, visible = True}
  | otherwise         = MetaTile {tile = tile m, visible = True}

-- | Resets tile to land if it is currently occupied by food or ant
-- | and makes the tile invisible.
clearMetaTile :: MetaTile -> MetaTile
clearMetaTile m
  | fOr (tile m) [isAnt, (==FoodTile), isDead] = MetaTile {tile = Land, visible = False}
  | otherwise = MetaTile {tile = tile m, visible = False}

--------------------------------------------------------------------------------
-- Immutable World -------------------------------------------------------------
--------------------------------------------------------------------------------
type World = Array Point MetaTile

colBound :: World -> Col
colBound = col.snd.bounds

rowBound :: World -> Row
rowBound = row.snd.bounds

-- | Accesses World using the modulus of the point
(%!) :: World -> Point -> MetaTile
(%!) w p = w ! (w %!% p)

-- | Takes the modulus of the point
(%!%) :: World -> Point -> Point
(%!%) w p =
  let modCol = 1 + colBound w
      modRow = 1 + rowBound w
      ixCol  = col p `mod` modCol
      ixRow  = row p `mod` modRow
  in (ixRow, ixCol)

-- | For debugging
renderWorld :: World -> String
renderWorld w = concatMap renderAssoc (assocs w)
  where
    maxCol = colBound w
    renderAssoc :: (Point, MetaTile) -> String
    renderAssoc a
      | col (fst a) == maxCol = renderTile (snd a) ++ "\n"
      | otherwise = renderTile (snd a)

--------------------------------------------------------------------------------
-- Norms and Metrics -----------------------------------------------------------
-- https://secure.wikimedia.org/wikipedia/en/wiki/Norm_(mathematics) -----------
--------------------------------------------------------------------------------
modDistance :: Int -- modulus
            -> Int -> Int -> Int
modDistance m x y = min ((x - y) `mod` m) ((y - x) `mod` m)

-- | Computes manhattan distance.
manhattan :: Point -- modulus point
          -> Point -> Point -> Int
manhattan mp p1 p2 =
  let rowd = modDistance (row mp) (row p1) (row p2)
      cold = modDistance (col mp) (col p1) (col p2)
  in rowd + cold

-- | Computes the square of the two norm.
twoNormSquared :: Point -> Int
twoNormSquared p = row p ^ (2::Int) + col p ^ (2::Int)

distance :: GameParams -> Point -> Point -> Int
distance gp p1 p2 =
  let mp = (rows gp, cols gp)
  in manhattan mp p1 p2

sumPoint :: Point -> Point -> Point
sumPoint x y = (row x + row y, col x + col y)

incPoint :: Point -> Point
incPoint = sumPoint (1,1)

modPoint :: Point -- modulus point
         -> Point -> Point
modPoint mp p = (row p `mod` row mp, col p `mod` col mp)

getPointCircle :: Int -- radius squared
               -> [Point]
getPointCircle r2 =
  let rx = truncate.sqrt.(fromIntegral::Int -> Double) $ r2
  in filter ((<=r2).twoNormSquared) $ (,) <$> [-rx..rx] <*> [-rx..rx]

--------------------------------------------------------------------------------
-- Ants ------------------------------------------------------------------------
--------------------------------------------------------------------------------
data Owner = Me | Enemy1 | Enemy2 | Enemy3 deriving (Show,Eq,Bounded,Enum)

data Ant = Ant
  { point :: Point
  , owner :: Owner
  } deriving (Show)

isMe, isEnemy :: Ant -> Bool
isMe = (==Me).owner
isEnemy = not.isMe

myAnts, enemyAnts :: [Ant] -> [Ant]
myAnts = filter isMe
enemyAnts = filter isEnemy

--------------------------------------------------------------------------------
-- Orders ----------------------------------------------------------------------
--------------------------------------------------------------------------------
data Direction = North | East | South | West deriving (Bounded, Eq, Enum)

instance Show Direction where
  show North = "N"
  show East  = "E"
  show South = "S"
  show West  = "W"

data Order = Order
  { ant :: Ant
  , direction :: Direction
  } deriving (Show)

move :: Direction -> Point -> Point
move dir p
  | dir == North = (row p - 1, col p)
  | dir == South = (row p + 1, col p)
  | dir == West  = (row p, col p - 1)
  | otherwise    = (row p, col p + 1)

passable :: World -> Order -> Bool
passable w order =
  let newPoint = move (direction order) (point $ ant order)
  in  tile (w %! newPoint) /= Water

issueOrder :: Order -> IO ()
issueOrder order = do
  let srow = (show . row . point . ant) order
      scol = (show . col . point . ant) order
      sdir = (show . direction) order
  putStrLn $ "o " ++ srow ++ " " ++ scol ++ " " ++ sdir

toOwner :: Int -> Owner
toOwner 0 = Me
toOwner 1 = Enemy1
toOwner 2 = Enemy2
toOwner _ = Enemy3

--------------------------------------------------------------------------------
-- MWorld ----------------------------------------------------------------------
--------------------------------------------------------------------------------
type MWorld = IOArray Point MetaTile

writeArrayMod :: MWorld -> Point -> MetaTile -> IO ()
writeArrayMod mw p mt = do
  bnds <- getBounds mw
  let np = modPoint (incPoint $ snd bnds) p
  writeArray mw np mt

readArrayMod :: MWorld -> Point -> IO MetaTile
readArrayMod mw p = do
  bnds <- getBounds mw
  let np = modPoint (incPoint $ snd bnds) p
  readArray mw np

--------------------------------------------------------------------------------
-- Updating Game ---------------------------------------------------------------
--------------------------------------------------------------------------------
type Food = Point

data GameState = GameState
  { world :: World
  , ants :: [Ant]
  , food :: [Food] -- call "food GameState" to return food list
  , startTime :: UTCTime
  }

data GameParams = GameParams
  { loadtime :: Int
  , turntime :: Int
  , rows :: Int
  , cols :: Int
  , turns :: Int
  , viewradius2 :: Int
  , attackradius2 :: Int
  , spawnradius2 :: Int
  , viewCircle :: [Point]
  , attackCircle :: [Point]
  , spawnCircle :: [Point]
  } deriving (Show)

-- | Used only for updating GameState
data MGameState = MGameState
  { mants :: [Ant]
  , mfood :: [Food] -- call "food GameState" to return food list
  }

setVisible :: MWorld -> Point -> IO ()
setVisible mw p = do
  mt <- readArrayMod mw p
  writeArrayMod mw p $ visibleMetaTile mt

addVisible :: MWorld
           -> [Point] -- viewPoints
           -> Point -- center point
           -> IO ()
addVisible mw vp p = do
  let vis = map (sumPoint p) vp
  mapM_ (setVisible mw) vis

addAnt :: [Point] -- viewPoints
       -> MWorld -> [Ant] -> Point -> Owner -> IO [Ant]
addAnt vp mw as p own = do
  writeArray mw p MetaTile {tile = AntTile own, visible = True}
  let as' = Ant {point = p, owner = own}:as
  when (own == Me) $ addVisible mw vp p
  return as'

addFood :: MWorld -> [Food] -> Point -> IO [Food]
addFood mw fs p = do
  writeArray mw p MetaTile {tile = FoodTile, visible = True}
  return $ p:fs

addDead :: [Point] -- viewPoints
        -> MWorld -> Point -> Owner -> IO ()
addDead vp mw p own = do
  writeArray mw p MetaTile {tile = Dead own, visible = True}
  when (own == Me) $ addVisible mw vp p

addWaterTile :: MWorld -> Point -> IO ()
addWaterTile mw p = writeArray mw p MetaTile {tile = Water, visible = True}

updateGameState :: [Point] -> MWorld -> MGameState -> String -> IO MGameState
updateGameState vp mw mgs s
  | "f" `isPrefixOf` s = do
      fs' <- addFood mw fs $ toPoint . tail $ s
      return MGameState { mfood = fs', mants = as }
  | "w" `isPrefixOf` s = do
      addWaterTile mw $ toPoint.tail $ s
      return mgs
  | "a" `isPrefixOf` s = do
      as' <- addAnt vp mw as (toPoint.init.tail $ s) (toOwner.digitToInt.last $ s)
      return MGameState { mants = as', mfood = fs}
  | "d" `isPrefixOf` s = do
      addDead vp mw (toPoint.init.tail $ s) (toOwner.digitToInt.last $ s)
      return mgs
  | otherwise = return mgs -- ignore line
  where
    toPoint :: String -> Point
    toPoint = tuplify2 . map read . words
    as = mants mgs
    fs = mfood mgs

updateGame :: [Point] -> MWorld -> MGameState -> IO GameState
updateGame vp mw mgs = do
  line <- getLine
  process line
  where
    process line
      | "go" `isPrefixOf` line   = do
          time <- getCurrentTime
          w <- unsafeFreeze mw
          return GameState { world = w
                           , ants = mants mgs
                           , food = mfood mgs
                           , startTime = time
                           }
      | otherwise = do
          mgs' <- updateGameState vp mw mgs line
          updateGame vp mw mgs'

initialGameState :: GameParams -> UTCTime -> GameState
initialGameState gp time =
  let w = listArray ((0,0), (rows gp - 1, cols gp - 1)) (repeat MetaTile {tile = Unknown, visible = False})
  in GameState {world = w, ants = [], food = [], startTime = time}

gatherParamInput :: IO [String]
gatherParamInput = gatherInput' []
  where
    gatherInput' :: [String] -> IO [String]
    gatherInput' xs = do
      line <- getLine
      if "ready" /= line
        then gatherInput' (line:xs)
        else return xs

createParams :: [(String, String)] -> GameParams
createParams s =
  let lookup' key = read $ fromJust $ lookup key s
      lt  = lookup' "loadtime"
      tt  = lookup' "turntime"
      rs  = lookup' "rows"
      cs  = lookup' "cols"
      ts  = lookup' "turns"

      vr2 = lookup' "viewradius2"
      vp = getPointCircle vr2

      ar2 = lookup' "attackradius2"
      ap = getPointCircle ar2

      sr2 = lookup' "spawnradius2"
      sp = getPointCircle sr2

  in GameParams { loadtime      = lt
                , turntime      = tt
                , rows          = rs
                , cols          = cs
                , turns         = ts
                , viewradius2   = vr2
                , attackradius2 = ar2
                , spawnradius2  = sr2
                , viewCircle    = vp
                , attackCircle  = ap
                , spawnCircle   = sp
                }

gameLoop :: GameParams -> GameState
         -> (GameParams -> GameState -> IO [Order])
         -> IO ()
gameLoop gp gs doTurn = do
  line <- getLine
  gameLoop' line
  where
    gameLoop' line
      | "turn" `isPrefixOf` line = do
          hPutStrLn stderr line
          w <- unsafeThaw $ world gs
          mw <- mapArray clearMetaTile w
          gsu <- updateGame (viewCircle gp) mw MGameState { mfood = [], mants = [] }
          orders <- doTurn gp gsu
          mapM_ issueOrder orders
          finishTurn
          gameLoop gp gsu doTurn
      | "end" `isPrefixOf` line = endGame
      | otherwise = gameLoop gp gs doTurn -- ignore line

game :: (GameParams -> GameState -> IO [Order]) -> IO ()
game doTurn = do
  paramInput <- gatherParamInput
  let gp = createParams $ map (tuplify2.words) paramInput
  currentTime <- getCurrentTime
  let gs = initialGameState gp currentTime
  finishTurn
  gameLoop gp gs doTurn

-- TODO this could be better
endGame :: IO ()
endGame = do
  players <- getLine
  hPutStrLn stderr $ "Number of players: " ++ (words players !! 1)
  scores <- getLine
  hPutStrLn stderr $ "Final scores: " ++ unwords (tail $ words scores)

-- | Tell engine that we have finished the turn or setting up.
finishTurn :: IO ()
finishTurn = putStrLn "go" >> hFlush stdout

-- vim: set expandtab:
