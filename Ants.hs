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

  -- TODO implement the following functions according to the starter pack guide
  --, direction
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

-- type synonyms
type Row = Int
type Col = Int
type Visible = Bool
type Point = (Row, Col)
type Food = Point
type MWorld = IOArray Point MetaTile
type World = Array Point MetaTile

-- Helper functions
fAnd :: a -> [a -> Bool] -> Bool
fAnd x = all ($x)

fOr :: a -> [a -> Bool] -> Bool
fOr x = any ($x)


colBound :: World -> Col
colBound = col . snd . bounds

rowBound :: World -> Row
rowBound = row . snd . bounds

-- Takes the modulus of the indices before accessing the array
(%!) :: World -> Point -> MetaTile
(%!) w p = w ! (w %!% p)

(%!%) :: World -> Point -> Point
(%!%) w p = 
  let modCol = 1 + colBound w
      modRow = 1 + rowBound w
      ixCol  = col p `mod` modCol
      ixRow  = row p `mod` modRow
  in (ixRow, ixCol)

row :: Point -> Row
row = fst

col :: Point -> Col
col = snd

-- Data objects

-- Objects appearing on the map
data Tile = MyTile 
          | Enemy1Tile 
          | Enemy2Tile 
          | Enemy3Tile 
          | Dead 
          | Land 
          | FoodTile 
          | Water 
          | Unknown 
          deriving (Show,Eq,Enum,Bounded)

data MetaTile = MetaTile
  { tile :: Tile
  , visible :: Visible
  } deriving (Show)

data Owner = Me | Enemy1 | Enemy2 | Enemy3 deriving (Show,Eq,Bounded,Enum)

data Ant = Ant
  { point :: Point
  , owner :: Owner
  } deriving (Show)

data Direction = North | East | South | West deriving (Bounded, Eq, Enum)

instance Show Direction where
  show North = "N"
  show East  = "E"
  show South = "S"
  show West  = "W"

-- Representation of an order
data Order = Order
  { ant :: Ant
  , direction :: Direction
  } deriving (Show)

data GameState = GameState
  { world :: World
  , ants :: [Ant]
  , food :: [Food] -- call "food GameState" to return food list
  , startTime :: UTCTime
  }

data MGameState = MGameState
  { mants :: [Ant]
  , mfood :: [Food] -- call "food GameState" to return food list
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
  , viewPoints :: [Point]
  } deriving (Show)

--------------- Tile functions -------------------
isAnt :: Tile -> Bool
isAnt t = any (==t) [MyTile .. Enemy3Tile]

renderTile :: MetaTile -> String
renderTile m 
  | tile m == MyTile = visibleUpper m 'm'
  | tile m == Enemy1Tile = visibleUpper m 'a'
  | tile m == Enemy2Tile = visibleUpper m 'b'
  | tile m == Enemy3Tile = visibleUpper m 'c'
  | tile m == Dead = visibleUpper m 'd'
  | tile m == Land = visibleUpper m 'l'
  | tile m == FoodTile = visibleUpper m 'f'
  | tile m == Water = visibleUpper m 'w'
  | otherwise = "*"
  where 
    visibleUpper :: MetaTile -> Char -> String
    visibleUpper mt c
      | visible mt = [toUpper c]
      | otherwise  = [c]
      
renderWorld :: World -> String
renderWorld w = concatMap renderAssoc (assocs w)
  where
    maxCol = colBound w
    renderAssoc :: (Point, MetaTile) -> String
    renderAssoc a 
      | col (fst a) == maxCol = renderTile (snd a) ++ "\n"
      | otherwise = renderTile (snd a)

modDistance :: Int -> Int -> Int -> Int
modDistance n x y = min ((x - y) `mod` n) ((y - x) `mod` n)

manhattan :: Point -- bound
          -> Point -> Point -> Int
manhattan bound p1 p2 = 
  let rowd = modDistance (row bound + 1) (row p1) (row p2)
      cold = modDistance (col bound + 1) (col p1) (col p2)
  in rowd + cold

oneNorm :: Point -> Int
oneNorm p = row p + col p

twoNormSquared :: Point -> Int
twoNormSquared p = row p ^ 2 + col p ^ 2

euclidSquare :: Point  -- bound
             -> Point -> Point -> Int
euclidSquare bound p1 p2 = 
  let rowd = modDistance (row bound + 1) (row p1) (row p2)
      cold = modDistance (col bound + 1) (col p1) (col p2)
  in (rowd ^ 2) + (cold ^ 2)

distance :: GameParams -> Point -> Point -> Int
distance gp l1 l2 =
  let maxRow = rows gp - 1
      maxCol = cols gp - 1
      rowDist = modDistance maxRow (row l1) (row l2)
      colDist = modDistance maxCol (col l1) (col l2)
  in rowDist + colDist

isMe :: Ant -> Bool
isMe a = owner a == Me

myAnts :: [Ant] -> [Ant]
myAnts = filter isMe

isEnemy :: Ant -> Bool
isEnemy = not . isMe

enemyAnts :: [Ant] -> [Ant]
enemyAnts = filter isEnemy

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

finishTurn :: IO ()
finishTurn = do
  putStrLn "go"
  hFlush stdout

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

ownerToTile :: Owner -> Tile
ownerToTile Me = MyTile
ownerToTile Enemy1 = Enemy1Tile
ownerToTile Enemy2 = Enemy2Tile
ownerToTile Enemy3 = Enemy2Tile

toOwner :: Int -> Owner
toOwner 0 = Me
toOwner 1 = Enemy1
toOwner 2 = Enemy2
toOwner _ = Enemy3

sumPoint :: Point -> Point -> Point
sumPoint x y = (row x + row y, col x + col y)

-- Sets the tile to visible
-- If the tile is still Unknown then it is land
visibleMetaTile :: MetaTile -> MetaTile
visibleMetaTile m 
  | tile m == Unknown = MetaTile {tile = Land, visible = True}
  | otherwise         = MetaTile {tile = tile m, visible = True}

-- Resets Tile to Land if it is currently occupied by food or ant
-- and makes the tile invisible
clearMetaTile :: MetaTile -> MetaTile
clearMetaTile m 
  | fOr (tile m) [isAnt, (==FoodTile), (==Dead)] = MetaTile {tile = Land, visible = False}
  | otherwise = MetaTile {tile = tile m, visible = False}

modPoint :: Point -- max bound
         -> Point -> Point
modPoint b p = (row p `mod` (1 + row b), col p `mod` (1 + col b))

writeArrayMod :: MWorld -> Point -> MetaTile -> IO ()
writeArrayMod mw p mt = do
  bnds <- getBounds mw
  let np = modPoint (snd bnds) p
  writeArray mw np mt


readArrayMod :: MWorld -> Point -> IO MetaTile
readArrayMod mw p = do
  bnds <- getBounds mw
  let np = modPoint (snd bnds) p
  readArray mw np

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

addAnt :: GameParams -> MWorld -> [Ant] -> Point -> Owner -> IO [Ant]
addAnt gp mw as p own = do
  writeArray mw p MetaTile {tile = ownerToTile own, visible = True}
  let as' = Ant {point = p, owner = own}:as
  when (own == Me) $ addVisible mw (viewPoints gp) p
  return as'

addFood :: MWorld -> [Food] -> Point -> IO [Food]
addFood mw fs p = do
  writeArray mw p MetaTile {tile = FoodTile, visible = True}
  return $ p:fs

addDead :: GameParams -> MWorld -> Point -> Owner -> IO ()
addDead gp mw p own = do
  writeArray mw p MetaTile {tile = Dead, visible = True}
  when (own == Me) $ addVisible mw (viewPoints gp) p

addWaterTile :: MWorld -> Point -> IO ()
addWaterTile mw p = writeArray mw p MetaTile {tile = Water, visible = True}

updateGameState :: GameParams -> MWorld -> MGameState -> String -> IO MGameState
updateGameState gp mw mgs s
  | "f" `isPrefixOf` s = do
      fs' <- addFood mw fs $ toPoint . tail $ s
      return MGameState { mfood = fs', mants = as }
  | "w" `isPrefixOf` s = do
      addWaterTile mw $ toPoint.tail $ s
      return mgs
  | "a" `isPrefixOf` s = do
      as' <- addAnt gp mw as (toPoint.init.tail $ s) (toOwner.digitToInt.last $ s)
      return MGameState { mants = as', mfood = fs}
  | "d" `isPrefixOf` s = do
      addDead gp mw (toPoint.init.tail $ s) (toOwner.digitToInt.last $ s)
      return mgs
  | otherwise = return mgs -- ignore line
  where
    toPoint :: String -> Point
    toPoint = tuplify2 . map read . words
    as = mants mgs
    fs = mfood mgs
    

updateGame :: GameParams -> MWorld -> MGameState -> IO GameState
updateGame gp mw mgs = do
  line <- getLine
  process line
  where 
    process line
      | "turn" `isPrefixOf` line = do
          hPutStrLn stderr line
          updateGame gp mw mgs
      | "go" `isPrefixOf` line   = do
          time <- getCurrentTime
          w <- unsafeFreeze mw
          return GameState { world = w
                           , ants = mants mgs
                           , food = mfood mgs
                           , startTime = time
                           }
      | otherwise = do
          mgs' <- updateGameState gp mw mgs line
          updateGame gp mw mgs'

timeRemaining :: GameState -> IO NominalDiffTime
timeRemaining gs = do
  timeNow <- getCurrentTime
  return $ timeNow `diffUTCTime` startTime gs

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
      ar2 = lookup' "attackradius2"
      sr2 = lookup' "spawnradius2"
      mx  = truncate $ sqrt $ fromIntegral vr2
      vp' = (,) <$> [-mx..mx] <*> [-mx..mx]
      vp  = filter (\p -> twoNormSquared p <= vr2) vp'
  in GameParams { loadtime      = lt
                , turntime      = tt
                , rows          = rs
                , cols          = cs
                , turns         = ts
                , viewradius2   = vr2
                , attackradius2 = ar2
                , spawnradius2  = sr2
                , viewPoints    = vp
                }

-- TODO this could be better
endGame :: IO ()
endGame = do
  players <- getLine
  hPutStrLn stderr $ "Number of players: " ++ (words players !! 1)
  scores <- getLine
  hPutStrLn stderr $ "Final scores: " ++ unwords (tail $ words scores)

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
          --let nw = cleanState gs
          w <- unsafeThaw $ world gs
          mw <- mapArray clearMetaTile w
          gsu <- updateGame gp mw MGameState { mfood = [], mants = [] }
          orders <- doTurn gp gsu
          {-putStrLn $ renderWorld $ world gsu-}
          hPutStrLn stderr $ show orders
          mapM_ issueOrder orders
          finishTurn
          gameLoop gp gsu doTurn
      | "end" `isPrefixOf` line = endGame
      | otherwise = gameLoop gp gs doTurn -- ignore line

game :: (GameParams -> GameState -> IO [Order]) -> IO ()
game doTurn = do
  paramInput <- gatherParamInput
  let gp = createParams $ map (tuplify2 . words) paramInput
  currentTime <- getCurrentTime
  let gs = initialGameState gp currentTime
  finishTurn -- signal done with setup
  gameLoop gp gs doTurn

-- vim: set expandtab:
