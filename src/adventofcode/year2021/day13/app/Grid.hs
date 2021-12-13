module Grid(Point, Grid, grid, renderGrid, gridElemAt, neighborCoords, neighbors, allCoords, foldAlongY, foldAlongX) where

import Utils (flatMap)
import qualified Data.Map as Map
import qualified Data.Array as Array
import qualified Data.Maybe as Maybe

type Point = (Int, Int)

type Grid k = Map.Map Point k
grid :: [k] -> Point -> Grid k
grid elems (width, height) = let start = (0,0)
                                 end =  (width-1, height-1)
                                 coords = Array.range(start, end)
                                 points = map(\(x, y) -> ((x, y), elems !! (y*width+x))) coords
                             in Map.fromList(points)

elemRender :: Show k => Grid k->Int->Int->String
elemRender grid y x = show(gridElemAt grid (x,y))

lineRender :: Show k => Grid k -> [Int] -> Int -> String
lineRender grid xRange y = foldl (++) "" (map(elemRender grid y) xRange)

renderGrid :: Show k => Grid k -> String
renderGrid grid 
    | Map.null grid = "EMPTY"
    | otherwise = let coords = Map.keys grid
                      maxX = maximum(map fst coords)
                      maxY = maximum(map snd coords)
                      xRange = Array.range(0, maxX)
                      yRange = Array.range(0, maxY)
                   in foldl (\a b -> a ++ "\n" ++ b) "" (map (lineRender grid xRange) yRange)


gridElemAt :: Grid k -> Point-> [k]
gridElemAt elems coord = Maybe.maybeToList(Map.lookup coord elems)


neighborCoords :: Point ->  [Point]
neighborCoords (x, y)  = let left    = [(x-1  , y)] 
                             top     = [(x    , y-1)]
                             right   = [(x+1  , y)]
                             bottom  = [(x    , y+1)]
                          in left ++ top ++ right ++ bottom


neighbors :: Point -> Grid k -> [k]
neighbors point grid = let coords = neighborCoords point
                       in flatMap (gridElemAt(grid)) coords

allCoords:: Grid k -> [(Point)]
allCoords elems = Map.keys elems

flipCoord :: Int -> Int -> Int
flipCoord pivot c = 2*pivot-c

foldCoord :: Int -> Int -> Int
foldCoord pivot c = if (c<pivot) then c else 2*pivot-c


foldAlongY:: (k -> k -> k) -> Int -> Grid k -> Grid k
foldAlongY mergeFunc yFlipAt grid = Map.mapKeysWith mergeFunc (\(x, y) -> (x, (foldCoord yFlipAt y))) grid


foldAlongX:: (k -> k -> k) -> Int -> Grid k -> Grid k
foldAlongX mergeFunc xFlipAt grid = Map.mapKeysWith mergeFunc (\(x, y) -> ((foldCoord xFlipAt x), y)) grid

