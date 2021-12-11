module Main where

import qualified Data.Map as Map
import Data.Array (range)
import Data.Maybe (fromJust, maybeToList)
import Debug.Trace (trace)

doWhile :: (a -> a) -> (a-> Bool) -> a -> a
doWhile doFunc whileFunc a = let next = doFunc(a)
                                 isContinue = whileFunc(next)
                             in if not(isContinue) then next else doWhile doFunc whileFunc next

flatMap :: ( b -> [a]) -> [b] -> [a]
flatMap mapFunc input = foldl (++) [] (map mapFunc input)

type Point = (Int, Int)

type Grid k = Map.Map Point k
grid :: [k] -> Point -> Grid k
grid elems (width, height) = let start = (0,0)
                                 end =  (width-1, height-1)
                                 coords = range(start, end)
                                 points = map(\(x, y) -> ((x, y), elems !! (y*width+x))) coords
                             in Map.fromList(points)

elemRender :: Show k => Grid k->Int->Int->String
elemRender grid y x = let elem = gridElemAt grid (x,y)
                      in case elem of [] -> " "
                                      [k] -> show k

lineRender :: Show k => Grid k -> [Int] -> Int -> String
lineRender grid xRange y = foldl (++) "" (map(elemRender grid y) xRange)

renderGrid :: Show k => Grid k -> String
renderGrid grid 
    | Map.null grid = "EMPTY"
    | otherwise = let coords = Map.keys grid
                      maxX = maximum(map fst coords)
                      maxY = maximum(map snd coords)
                      xRange = range(0, maxX)
                      yRange = range(0, maxY)
                   in foldl (\a b -> a ++ "\n" ++ b) "" (map (lineRender grid xRange) yRange)


gridElemAt :: Grid k -> Point-> [k]
gridElemAt elems coord = maybeToList(Map.lookup coord elems)


neighborCoords :: Point ->  [Point]
neighborCoords (x, y)  = let left        = [(x-1  , y)] 
                             top         = [(x    , y-1)]
                             right       = [(x+1  , y)]
                             bottom      = [(x    , y+1)]
                             topleft     = [(x-1  , y-1)] 
                             topright    = [(x+1  , y-1)]
                             bottomleft  = [(x-1  , y+1)]
                             bottomright = [(x+1  , y+1)]
                          in left ++ top ++ right ++ bottom ++ topleft ++ topright ++ bottomleft ++ bottomright


neighbors :: Point -> Grid k -> [k]
neighbors point grid = let coords = neighborCoords point
                       in flatMap (gridElemAt(grid)) coords

allCoords:: Grid k -> [(Point)]
allCoords elems = Map.keys elems

input :: Grid Int
--input = grid [5,4,8,3,1,4,3,2,2,3,2,7,4,5,8,5,4,7,1,1,5,2,6,4,5,5,6,1,7,3,6,1,4,1,3,3,6,1,4,6,6,3,5,7,3,8,5,4,7,8,4,1,6,7,5,2,4,6,4,5,2,1,7,6,8,4,1,7,2,1,6,8,8,2,8,8,1,1,3,4,4,8,4,6,8,4,8,5,5,4,5,2,8,3,7,5,1,5,2,6] (10,10)
input = grid [4,7,4,3,3,7,8,3,1,8,4,6,6,4,2,1,2,8,4,4,2,5,3,5,6,6,7,8,8,4,3,2,7,3,3,6,3,8,6,1,2,2,8,2,4,3,2,6,1,2,2,1,6,6,6,1,2,1,3,4,3,7,7,6,3,3,4,5,1,3,8,1,2,3,8,5,2,5,8,3,8,1,8,1,7,8,6,6,8,5,4,3,6,2,5,3,3,1,7,4] (10,10)

mutateOne :: Grid Int -> Point -> Int -> Int
mutateOne grid point value = let surrounding = neighbors point grid
                                 numFlashing = length(filter (==9) surrounding)
                             in value +1 + numFlashing

flashEm :: Grid Int -> Grid Int
flashEm grid = Map.map (\_ -> 0) grid


resolveFlashes :: (Grid Int , Grid Int) -> (Grid Int, Grid Int)
resolveFlashes (flashing, unresolved) = let nextFlashing = Map.filter (>=10) unresolved
                                        in if(Map.null nextFlashing) then (flashing, unresolved)
                                           else let flashed = flashEm nextFlashing
                                                    flashCoords = Map.keys flashed
                                                    remaining = foldl (\grid point -> Map.delete point grid) unresolved flashCoords
                                                    propogatedCoords = flatMap neighborCoords flashCoords
                                                    propogatedGrid = foldl (\grid point -> Map.adjust (+1) point grid) remaining propogatedCoords
                                                in resolveFlashes(Map.union flashing flashed, propogatedGrid)

mutate :: Grid Int -> (Int, Grid Int)
mutate grid = let increasedEnergy = Map.map (+1) grid
                  (flashing, notFlashing) = resolveFlashes (Map.empty, increasedEnergy)
                  numFlashing = length flashing 
                  afterFlash = Map.union flashing notFlashing
              in trace(renderGrid afterFlash)(numFlashing, afterFlash)


main :: IO ()
main = do
    putStrLn(renderGrid input)
    let (numFlashing, mutated) = foldl (\(accNumFlashing, grid) _ -> let (numFlashing, mutated) = mutate grid
                                                                   in  (accNumFlashing + numFlashing, mutated)) (0, input) [0..99]
    
    putStrLn(renderGrid mutated)
    putStrLn(show numFlashing)

    let (_,_,numSteps) = doWhile (\(_, grid, numSteps) -> let (numFlashing, nextMutation) = mutate grid in (numFlashing, nextMutation, numSteps+1))
                                 (\(numFlashing, _, _) -> numFlashing < 100)
                                 (0, input, 0)
    putStrLn(show numSteps)
