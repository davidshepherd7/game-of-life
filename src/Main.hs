module Main(main) where

import Data.Function((&))
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Map.Strict as Map
import System.Random

width, height, windowOffset, pixelSize :: Int
windowOffset = 100
fps = 2
pixelSize = 10
npixel = 30
width = pixelSize * npixel + 50
height = width


window = InWindow "Conway" (width, height) (windowOffset, windowOffset)

background :: Color
background = dark $ dark $ dark blue

pixelColour :: Bool -> Color
pixelColour True = white
pixelColour False = black

initialGrid = cycle [False] & listToGrid

initialState :: GameOfLife
initialState = Game initialGrid True

main :: IO ()
main = playIO window background fps initialState draw handleEvents update

type Grid = Map.Map (Int, Int) Bool

data GameOfLife = Game
  { grid :: Grid
  , editMode :: Bool
  } deriving Show


pixelToPosition :: (Int, Int) -> (Float, Float)
pixelToPosition (i, j) = (
  fromIntegral (i * pixelSize) - 150,
  fromIntegral ((npixel - j) * pixelSize) - 150
  )

positionToPixel :: (Float, Float) -> (Int, Int)
positionToPixel (x, y) = (
  round ((x + 150) / fromIntegral pixelSize),
  npixel - (round ((y + 150) / fromIntegral pixelSize))
  )


draw :: GameOfLife -> IO Picture
draw game = return $ pictures (map drawSquare (Map.toList (grid game)) ++ renderEditMode game)

drawSquare :: ((Int, Int), Bool) -> Picture
drawSquare ((i, j), alive) = rectangleSolid (fromIntegral pixelSize) (fromIntegral pixelSize) &
  color (pixelColour alive) &
  uncurry translate (pixelToPosition (i, j))

renderEditMode :: GameOfLife -> [Picture]
renderEditMode Game {editMode=True} = [text "Edit" & color white & translate (-30) 115 & scale 0.25 0.25]
renderEditMode game = []


listToGrid :: [Bool] -> Grid
listToGrid list = list & take (30 * 31) & indexList & makeLookup
  where makeLookup grid = Map.fromList $ map (\(i, j, x) -> ((i, j), x)) grid
        indicies = [(i, j) | i <- [0..npixel], j <- [0..npixel]]
        indexList list = zip indicies list & map (\((i, j), x) -> (i, j, x))


randomGrid gen = (randoms gen :: [Bool]) & listToGrid

handleEvents :: Event -> GameOfLife -> IO GameOfLife
handleEvents (EventKey (Char 'p') Down _ _) game = return game { editMode = not (editMode game) }
handleEvents (EventKey (Char 'r') Down _ _) game = return game { grid = initialGrid }
handleEvents (EventKey (Char 't') Down _ _) game = do
  g <- newStdGen
  return game { grid = randomGrid g }
handleEvents (EventKey (MouseButton LeftButton) Down _ pos) game = return $
  if (editMode game)
  then game { grid = doClick pos (grid game) }
  else game
handleEvents _ game = return game

doClick :: (Float, Float) -> Grid -> Grid
doClick pos grid = newValue
  where pixelPos = positionToPixel pos
        newValue = case (Map.lookup pixelPos grid) of
          Just x -> Map.insert pixelPos (not x) grid
          Nothing -> grid



update :: Float -> GameOfLife -> IO GameOfLife
update _ game = return (updateLiveness game)

updateLiveness game = if not (editMode game)
                      then game { grid = stepLiveness (grid game) }
                      else game

stepLiveness :: Grid -> Grid
stepLiveness grid = Map.mapWithKey (alive grid) grid

existsAndAlive :: Maybe Bool -> Bool
existsAndAlive (Just x) = x
existsAndAlive Nothing = False

countTrue :: [Bool] -> Int
countTrue xs = length $ filter id xs

neighbourPositions = [(-1, -1), (-1,  0), (-1,  1),
                      ( 0, -1),           ( 0,  1),
                      ( 1, -1), ( 1,  0), ( 1,  1)]

livingNeighbours :: Map.Map (Int, Int) Bool -> (Int, Int) -> Int
livingNeighbours lookup (i, j) = neighbourPositions &
  map (\(di, dj) -> (i + di, j + dj)) &
  map (\k -> Map.lookup k lookup) &
  map existsAndAlive &
  countTrue

alive :: Map.Map (Int, Int) Bool -> (Int, Int) -> Bool -> Bool
alive lookup (i, j) wasAlive = isAlive
  where
    isAlive = (wasAlive && (nLivingNeighbours == 2 || nLivingNeighbours == 3)) ||
              ((not wasAlive) && nLivingNeighbours == 3)
    nLivingNeighbours = livingNeighbours lookup (i, j)
