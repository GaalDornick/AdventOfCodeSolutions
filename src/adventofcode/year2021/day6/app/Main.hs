module Main where

import Debug.Trace (trace)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

input :: [Int]
--input = [3,4,3,1,2]
input=[5,1,1,1,3,5,1,1,1,1,5,3,1,1,3,1,1,1,4,1,1,1,1,1,2,4,3,4,1,5,3,4,1,1,5,1,2,1,1,2,1,1,2,1,1,4,2,3,2,1,4,1,1,4,2,1,4,5,5,1,1,1,1,1,2,1,1,1,2,1,5,5,1,1,4,4,5,1,1,1,3,1,5,1,2,1,5,1,4,1,3,2,4,2,1,1,4,1,1,1,1,4,1,1,1,1,1,3,5,4,1,1,3,1,1,1,2,1,1,1,1,5,1,1,1,4,1,4,1,1,1,1,1,2,1,1,5,1,2,1,1,2,1,1,2,4,1,1,5,1,3,4,1,2,4,1,1,1,1,1,4,1,1,4,2,2,1,5,1,4,1,1,5,1,1,5,5,1,1,1,1,1,5,2,1,3,3,1,1,1,3,2,4,5,1,2,1,5,1,4,1,5,1,1,1,1,1,1,4,3,1,1,3,3,1,4,5,1,1,4,1,4,3,4,1,1,1,2,2,1,2,5,1,1,3,5,2,1,1,1,1,1,1,1,4,4,1,5,4,1,1,1,1,1,2,1,2,1,5,1,1,3,1,1,1,1,1,1,1,1,1,1,2,1,3,1,5,3,3,1,1,2,4,4,1,1,2,1,1,3,1,1,1,1,2,3,4,1,1,2]

-- --------- Generic Histogram

--Histogram is a Map, key is the item and value is the number of time item has occurred
type Histogram k = Map.Map k Integer


histogram ::  Ord k => [k] -> Histogram k
histogram as = foldl (updateHistogram(1)) Map.empty as

updateHistogram :: Ord k => Integer -> Histogram k -> k -> Histogram k
updateHistogram n acc a = Map.insertWith(+) a n acc


count :: Ord k => k -> Histogram k -> Integer
count a m = Map.findWithDefault 0 a m

total :: Ord k => Histogram k -> Integer
total m = Map.foldl (+) 0 m 


-- ----------END generic histogram



type Fishes = Histogram Int 




toFishes :: [Int] -> Fishes
toFishes inp = histogram inp 

nextLife :: Fishes -> Fishes
nextLife fishes = Map.mapKeysWith (+) (\n -> if n==0 then 6 else n-1) fishes

newFish :: Fishes -> Fishes
newFish fishes = let num0 = Map.lookup 0 fishes
                 in case num0 of Just(numNewFishes) -> Map.insert 9 numNewFishes fishes
                                 Nothing -> fishes 

nextGen :: (Fishes,Int) -> (Fishes,Int)
nextGen (fishes, iter) = (nextLife(newFish(fishes)) , iter-1)

isDone :: (Fishes,Int) -> Bool
isDone (_,iter) = iter>0

doWhile :: a -> (a -> a) -> (a -> Bool) -> a
doWhile acc doFunc whileFunc 
    | not(whileFunc(acc)) = acc
    | otherwise = doWhile(doFunc(acc)) doFunc whileFunc

run :: Fishes -> Int -> IO ()
run fishes n = do
    let (fishesN, _) = doWhile (fishes,n) nextGen isDone
    let totalFishesN = total fishesN 
    putStrLn (show(n) ++ ":" ++ show(fishesN) ++ " - " ++ show(totalFishesN))

main :: IO ()
main = do

    let fishes = toFishes input
    putStrLn (show(fishes))
    
    run fishes 80
    run fishes 256
