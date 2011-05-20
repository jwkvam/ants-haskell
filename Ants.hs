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

import Data.Array
import Data.List (isPrefixOf)
import Data.Char (digitToInt, toUpper)
import Data.Maybe (fromJust)
import Control.Applicative
import Data.Time.Clock
import System.IO

-- type synonyms
type Row = Int
type Col = Int
type Visible = Bool
type Point = (Row, Col)
type Food = Point
type World = Array Point MetaTile

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

addFood :: GameState -> Point -> GameState
addFood gs loc = 
  let newFood = loc:food gs
      newWorld = world gs // [(loc, MetaTile {tile = FoodTile, visible = True})]
  in GameState {world = newWorld, ants = ants gs, food = newFood, startTime = startTime gs}

sumPoint :: Point -> Point -> Point
sumPoint x y = (row x + row y, col x + col y)

addVisible :: World 
           -> [Point] -- viewPoints
           -> Point -- location
           -> World
addVisible w vp p = 
  let vis = map (sumPoint p) vp
      vtuple :: Point -> (Point, MetaTile)
      vtuple pt = (w %!% pt, visibleMetaTile $ w %! pt)
  in w // map vtuple vis

addAnt :: GameParams -> GameState -> Point -> Owner -> GameState
addAnt gp gs p own = 
  let newAnts   = Ant {point = p, owner = own}:ants gs
      newWorld' = if own == Me
                    then addVisible (world gs) (viewPoints gp) p
                    else world gs
      newWorld  = newWorld' // [(p, MetaTile {tile = ownerToTile own, visible = True})]
  in GameState {world = newWorld, ants = newAnts, food = food gs, startTime = startTime gs}

addDead :: GameParams -> GameState -> Point -> Owner -> GameState
addDead gp gs p own =
  let newWorld' = if own == Me 
                    then addVisible (world gs) (viewPoints gp) p
                    else world gs
      newWorld = newWorld' // [(p, MetaTile {tile = Dead, visible = True})]
  in GameState {world = newWorld, ants = ants gs, food = food gs, startTime = startTime gs}

-- if replacing a visible tile it should be kept visible
addWorldTile :: GameState -> Tile -> Point -> GameState
addWorldTile gs t p =
  let newWorld = world gs // [(p, MetaTile {tile = t, visible = True})]
  in GameState {world = newWorld, ants = ants gs, food = food gs, startTime = startTime gs}

initialGameState :: GameParams -> UTCTime -> GameState
initialGameState gp time =
  let w = listArray ((0,0), (rows gp - 1, cols gp - 1)) (repeat MetaTile {tile = Unknown, visible = False})
  in GameState {world = w, ants = [], food = [], startTime = time}

updateGameState :: GameParams -> GameState -> String -> GameState
updateGameState gp gs s
  | "f" `isPrefixOf` s = addFood gs $ toPoint . tail $ s
  | "w" `isPrefixOf` s = addWorldTile gs Water $ toPoint . tail $ s
  | "a" `isPrefixOf` s = addAnt gp gs (toPoint . init . tail $ s) (toOwner . digitToInt . last $ s)
  | "d" `isPrefixOf` s = addDead gp gs (toPoint . init . tail $ s) (toOwner . digitToInt . last $ s)
  | otherwise = gs -- ignore line
  where
    toPoint :: String -> Point
    toPoint = tuplify2 . map read . words

updateGame :: GameParams -> GameState -> IO GameState
updateGame gp gs = do
  line <- getLine
  process line
  where 
    process line
      | "turn" `isPrefixOf` line = do
          hPutStrLn stderr line
          updateGame gp gs 
      | "go" `isPrefixOf` line   = do
          currentTime <- getCurrentTime
          return GameState {world = world gs
                           , ants = ants gs
                           , food = food gs
                           , startTime = currentTime
                           }
      | otherwise = updateGame gp $ updateGameState gp gs line

-- Sets the tile to visible
-- If the tile is still Unknown then it is land
visibleMetaTile :: MetaTile -> MetaTile
visibleMetaTile m 
  | tile m == Unknown = MetaTile {tile = Land, visible = True}
  | otherwise         = MetaTile {tile = tile m, visible = True}

fAnd :: a -> [a -> Bool] -> Bool
fAnd x = all ($x)

fOr :: a -> [a -> Bool] -> Bool
fOr x = any ($x)

-- Resets Tile to Land if it is currently occupied by food or ant
-- and makes the tile invisible
clearMetaTile :: MetaTile -> MetaTile
clearMetaTile m 
  | fOr (tile m) [isAnt, (==FoodTile), (==Dead)] = MetaTile {tile = Land, visible = False}
  | otherwise = MetaTile {tile = tile m, visible = False}

-- Clears ants and food and sets tiles to invisible
cleanState :: GameState -> GameState
cleanState gs = 
  GameState {world = nw, ants = [], food = [], startTime = startTime gs}
  where 
    w = world gs
    invisibles = map clearMetaTile $ elems w
    nw = listArray (bounds w) invisibles

timeRemaining :: GameState -> IO NominalDiffTime
timeRemaining gs = do
  timeNow <- getCurrentTime
  return $ timeNow `diffUTCTime` startTime gs

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

endGame :: IO ()
endGame = do
  players <- getLine
  hPutStrLn stderr $ "Number of players: " ++ (words players !! 1)
  scores <- getLine
  hPutStrLn stderr $ "Final scores: " ++ unwords (tail $ words scores)
  -- TODO print 

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
          let gsc = cleanState gs
          gsu <- updateGame gp gsc
          orders <- doTurn gp gsu
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
