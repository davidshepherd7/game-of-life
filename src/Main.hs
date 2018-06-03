module Main(main) where

import Data.Function((&))
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Interact
import qualified Data.Map.Strict as Map

width, height, offset, pixelSize :: Int
width = 350
height = 350
offset = 100
fps = 2
pixelSize = 10
npixel = 30

window = InWindow "Conway" (width, height) (offset, offset)

background :: Color
background = dark $ dark $ dark blue

main :: IO ()
main = play window background fps initialState draw handleEvents update

data GameOfLife = Game
  { grid :: [(Int, Int, Bool)]
  , editMode :: Bool
  } deriving Show

enumerateRow :: (Int, [Bool]) -> [(Int, Int, Bool)]
enumerateRow (j, row) = (flip map) (zip [0..npixel] row) (\(i, x) -> (i, j, x))

-- enumerateGrid :: [[Bool]] -> [(Int, Int, Bool)]
-- enumerateGrid grid = (zip [0..] grid) >>= enumerateRow

draw :: GameOfLife -> Picture
draw game = pictures (map centerSquare $ map drawSquare (grid game) ++ renderEditMode game)

pixelColour True = white
pixelColour False = black

drawSquare :: (Int, Int, Bool) -> Picture
drawSquare (i, j, alive) = translate (fromIntegral ((fromIntegral i) * pixelSize))
                                     (fromIntegral ((fromIntegral (npixel - j)) * pixelSize)) $
  color (pixelColour alive) $
  rectangleSolid (fromIntegral pixelSize) (fromIntegral pixelSize)

renderEditMode :: GameOfLife -> [Picture]
renderEditMode Game {editMode=True} = [text "Edit" & color white & translate (-30) 115 & scale 0.25 0.25]
renderEditMode game = []
matrixToIndexed :: [[Bool]] -> [(Int, Int, Bool)]
matrixToIndexed grid = map (\(i, j, x) -> (fromIntegral i, fromIntegral j, x)) ((zip [0..npixel] grid) >>= enumerateRow)

centerSquare :: Picture -> Picture
centerSquare = translate (-150) (-150)

initialState :: GameOfLife
initialState = Game (matrixToIndexed [
  [False, False, False, False, False, False, False, True, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, True],
  [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, True,  True,  False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, True,  True,  False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, False, False, True,  True,  False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, False, False, True,  True,  False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
  [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False]
  ]) True

handleEvents :: Event -> GameOfLife -> GameOfLife
handleEvents (EventKey (Char 'p') Down _ _) game = game { editMode = not (editMode game) }
handleEvents _ game = game


update _ game = (updateLiveness) game

updateLiveness game = if not (editMode game)
                      then game { grid = stepLiveness (grid game) }
                      else game

makeLookup :: [(Int, Int, Bool)] -> Map.Map (Int, Int) Bool
makeLookup grid = Map.fromList $ map (\(i, j, x) -> ((i, j), x)) grid

stepLiveness :: [(Int, Int, Bool)] -> [(Int, Int, Bool)]
stepLiveness grid = map (alive (makeLookup grid)) grid

existsAndAlive :: Maybe Bool -> Bool
existsAndAlive (Just x) = x
existsAndAlive Nothing = False

countTrue :: [Bool] -> Int
countTrue xs = length $ filter id xs

neighbourPositions = [(-1, -1), (-1,  0), (-1,  1),
                      ( 0, -1),           ( 0,  1),
                      ( 1, -1), ( 1,  0), ( 1,  1)
                      ]

livingNeighbours :: Map.Map (Int, Int) Bool -> (Int, Int) -> Int
livingNeighbours lookup (i, j) = neighbourPositions &
  map (\(di, dj) -> (i + di, j + dj)) &
  map (\k -> Map.lookup k lookup) &
  map existsAndAlive &
  countTrue

-- TODO: handle i,j better
alive :: Map.Map (Int, Int) Bool -> (Int, Int, Bool) -> (Int, Int, Bool)
alive lookup (i, j, wasAlive) = (i, j, isAlive)
  where
    isAlive = (wasAlive && (nLivingNeighbours == 2 || nLivingNeighbours == 3)) ||
              ((not wasAlive) && nLivingNeighbours == 3)
    nLivingNeighbours = livingNeighbours lookup (i, j)
