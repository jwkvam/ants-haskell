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
  , myAnts
  , enemyAnts
  , passable
  , distance
  , timeRemaining

    -- main function
  , game
  ) where

import Control.Applicative
import Control.Monad.ST

import Data.Array
import Data.Array.ST
import Data.List (isPrefixOf, foldl')
import Data.Char (digitToInt, toUpper)
import Data.Maybe (fromJust)

import Data.Time.Clock
import System.IO

import Util

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

-- | Elements of the world
data MetaTile = MetaTile
  { tile :: Tile
  , visible :: Bool
  } deriving (Show)

isAnt, isDead, isAntEnemy, isDeadEnemy :: Tile -> Bool
isAnt (AntTile _) = True
isAnt _ = False

isDead (Dead _) = True
isDead _ = False

isAntEnemy (AntTile (Enemy _)) = True
isAntEnemy _ = False

isDeadEnemy (Dead (Enemy _)) = True
isDeadEnemy _ = False

-- | For debugging
renderTile :: MetaTile -> String
renderTile m
  | tile m == AntTile Me = visibleUpper m 'm'
  | isAntEnemy $ tile m = visibleUpper m 'e'
  | tile m == Dead Me = visibleUpper m 'd'
  | isDeadEnemy $ tile m = visibleUpper m 'd'
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
--   and makes the tile invisible.
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
modDistance m x y = 
  let a = abs $ x - y
  in min a (m - a)

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
data Owner = Me | Enemy Int deriving (Show,Eq)

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
toOwner a = Enemy a

--------------------------------------------------------------------------------
-- Updating Game ---------------------------------------------------------------
--------------------------------------------------------------------------------
type MWorld s = STArray s Point MetaTile
type Food = Point

data GameState = GameState
  { world :: World
  , ants :: [Ant] -- call "ants GameState" to all ants
  , food :: [Food] -- call "food GameState" to all food
  , startTime :: UTCTime
  }

data GameParams = GameParams
  { loadtime :: Int
  , turntime :: Int
  , rows :: Int
  , cols :: Int
  , turns :: Int
  , playerSeed :: Int
  , viewradius2 :: Int
  , attackradius2 :: Int
  , spawnradius2 :: Int
  , viewCircle :: [Point]
  , attackCircle :: [Point]
  , spawnCircle :: [Point]
  } deriving (Show)

setVisible :: MWorld s -> Point -> ST s ()
setVisible mw p = do
  bnds <- getBounds mw
  let np = modPoint (incPoint $ snd bnds) p
  modifyWorld mw visibleMetaTile np

addVisible :: World
           -> [Point] -- viewPoints
           -> Point -- center point
           -> World
addVisible w vp p = 
  runSTArray $ do 
    w' <- unsafeThaw w
    mapM_ (setVisible w' . sumPoint p) vp
    return w'

updateGameState :: [Point] -> GameState -> String -> GameState
updateGameState vp gs s
  | "f" `isPrefixOf` s = -- add food
      let p = toPoint.tail $ s
          fs' = p:food gs
          nw = writeTile (world gs) p FoodTile
      in GameState nw (ants gs) fs' (startTime gs)
  | "w" `isPrefixOf` s = -- add water
      let p = toPoint.tail $ s
          nw = writeTile (world gs) p Water
      in GameState nw (ants gs) (food gs) (startTime gs)
  | "a" `isPrefixOf` s = -- add ant
      let own = toOwner.digitToInt.last $ s
          p = toPoint.init.tail $ s
          as' = Ant { point = p, owner = own}:ants gs
          nw = writeTile (world gs) p $ AntTile own
          nw' = if own == Me then addVisible nw vp p else nw
      in GameState nw' as' (food gs) (startTime gs)
  | "d" `isPrefixOf` s = -- add dead ant
      let own = toOwner.digitToInt.last $ s
          p = toPoint.init.tail $ s
          nw = writeTile (world gs) p $ Dead own
      in GameState nw (ants gs) (food gs) (startTime gs)
  | otherwise = gs -- ignore line
  where
    toPoint :: String -> Point
    toPoint = tuplify2.map read.words
    writeTile w p t = runSTArray $ do
      w' <- unsafeThaw w
      writeArray w' p MetaTile {tile = t, visible = True}
      return w'

initialWorld :: GameParams -> World
initialWorld gp = listArray ((0,0), (rows gp - 1, cols gp - 1)) $ repeat MetaTile {tile = Unknown, visible = False}

createParams :: [(String, String)] -> GameParams
createParams s =
  let lookup' key = read $ fromJust $ lookup key s
      vr2 = lookup' "viewradius2"
      ar2 = lookup' "attackradius2"
      sr2 = lookup' "spawnradius2"
      vp = getPointCircle vr2
      ap = getPointCircle ar2
      sp = getPointCircle sr2
  in GameParams { loadtime      = lookup' "loadtime"
                , turntime      = lookup' "turntime"
                , rows          = lookup' "rows"
                , cols          = lookup' "cols"
                , turns         = lookup' "turns"
                , playerSeed    = lookup' "player_seed"
                , viewradius2   = vr2
                , attackradius2 = ar2
                , spawnradius2  = sr2
                , viewCircle    = vp
                , attackCircle  = ap
                , spawnCircle   = sp
                }

modifyWorld :: MWorld s -> (MetaTile -> MetaTile) -> Point -> ST s ()
modifyWorld mw f p = do
  e' <- readArray mw p
  e' `seq` writeArray mw p (f e') -- !IMPORTANT! seq is necessary to avoid space leaks

mapWorld :: (MetaTile -> MetaTile) -> World -> World
mapWorld f w = runSTArray $ do
  mw <- unsafeThaw w
  mapM_ (modifyWorld mw f) (indices w)
  return mw

gameLoop :: GameParams 
         -> (GameState -> IO [Order])
         -> World
         -> [String] -- input
         -> IO ()
gameLoop gp doTurn w (line:input)
  | "turn" `isPrefixOf` line = do
      hPutStrLn stderr line
      time <- getCurrentTime
      let cs = break (isPrefixOf "go") input
          gs = foldl' (updateGameState $ viewCircle gp) (GameState w [] [] time) (fst cs)
      orders <- doTurn gs
      mapM_ issueOrder orders
      finishTurn
      gameLoop gp doTurn (mapWorld clearMetaTile $ world gs) (tail $ snd cs) -- clear world for next turn
  | "end" `isPrefixOf` line = endGame input
  | otherwise = gameLoop gp doTurn w input
gameLoop _ _ _ [] = endGame []

game :: (GameParams -> GameState -> IO [Order]) -> IO ()
game doTurn = do
  content <- getContents
  let cs = break (isPrefixOf "ready") $ lines content
      gp = createParams $ map (tuplify2.words) (fst cs)
  finishTurn
  gameLoop gp (doTurn gp) (initialWorld gp) (tail $ snd cs)

-- TODO this could be better
endGame :: [String] -> IO ()
endGame input = do
  hPutStrLn stderr "end of game"
  mapM_ (hPutStrLn stderr) input

-- | Tell engine that we have finished the turn or setting up.
finishTurn :: IO ()
finishTurn = do
  putStrLn "go" 
  hFlush stdout

-- vim: set expandtab:
