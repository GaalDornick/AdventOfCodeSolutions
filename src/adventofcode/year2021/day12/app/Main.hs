module Main where

import qualified Data.Set as Set
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.Char (isUpper)
import Data.List (elem)

input :: [String] 
--input = ["start-A","start-b","A-c","A-b","b-d","A-end","b-end"]
--input = ["dc-end","HN-start","start-kj","dc-start","dc-HN","LN-dc","HN-end","kj-sa","kj-HN","kj-dc"]
--input = ["fs-end","he-DX","fs-he","start-DX","pj-DX","end-zg","zg-sl","zg-pj","pj-he","RW-he","fs-DX","pj-RW","zg-RW","start-pj","he-WI","zg-he","pj-fs","start-RW"]
input = ["rf-RL","rf-wz","wz-RL","AV-mh","end-wz","end-dm","wz-gy","wz-dm","cg-AV","rf-AV","rf-gy","end-mh","cg-gy","cg-RL","gy-RL","VI-gy","AV-gy","dm-rf","start-cg","start-RL","rf-mh","AV-start","qk-mh","wz-mh"]
flatMap :: ( b -> [a]) -> [b] -> [a]
flatMap mapFunc input = foldl (++) [] (map mapFunc input)

type Graph k = (Set.Set k, Map.Map k [k])

emptyGraph :: Graph k
emptyGraph = (Set.empty, Map.empty)

graphSegmentToFragment :: Ord k => (k, k) -> Graph k
graphSegmentToFragment (n0, n1) = (Set.fromList([n0,n1]), Map.fromList([(n0,[n1]), (n1, [n0])]) )

mergeGraph :: Ord k => Graph k -> Graph k -> Graph k
mergeGraph one two = let resultNodes = nodes(one) `Set.union` nodes(two)
                         resultPaths = Map.unionWith (++) (paths(one)) (paths(two))
                     in (resultNodes, resultPaths)

graph :: Ord k => [(k,k)] ->Graph k
graph graphSegments = let fragments = map graphSegmentToFragment graphSegments
                      in foldl mergeGraph emptyGraph fragments

nodes :: Graph k -> Set.Set k
nodes graph = fst graph

paths :: Graph k -> Map.Map k [k]
paths graph = snd graph 

isLeaf :: Ord k => k -> Graph k -> Bool
isLeaf k (node, paths) = isNothing(Map.lookup k paths)

children :: Ord k => k -> Graph k -> [k]
children k graph = Map.findWithDefault [] k (paths(graph))

removeNode ::Ord k =>  Graph k -> k -> Graph k
removeNode (nodes, paths) node = let newNodes = Set.filter (/=node) nodes
                                     newPaths =  Map.map (filter(/=node)) paths
                                 in (newNodes, newPaths) 


cave :: Graph String
cave = graph( map (\inp -> let parts = splitOn "-" inp in (head(parts), last(parts))) input)

isBig :: String -> Bool
isBig s = foldl (&&) True (map (isUpper) s)

isEnd :: String -> Bool
isEnd "end" = True
isEnd _     = False

isStart :: String -> Bool
isStart "start" = True
isStart _       = False

visitNode :: Graph String -> String -> [[String]]
visitNode  graph s
    | isEnd s   = [[s]] -- found the end
    | isLeaf s graph = [] -- can't go further ant didn't reach the end
    | otherwise = let nextGraph = if (isBig s) then graph else (removeNode graph s)
                      nextPaths = flatMap (visitNode(nextGraph))(children s graph)
                  in map (\nextPath -> s:nextPath) (filter (\nextPath -> length(nextPath)>0) nextPaths)

updateGraphAfterVisit :: String -> Graph String -> Int -> Graph String
updateGraphAfterVisit node graph visitCount 
    | isStart node = removeNode graph node
    | isBig node = graph
    | visitCount < 2 = graph
    | otherwise = removeNode graph node 

visitNode2 :: Graph String -> Map.Map String Int -> String  -> [[String]]
visitNode2  graph numVisits s
    | isEnd s   = [[s]] -- found the end
    | isLeaf s graph = [] -- can't go further ant didn't reach the end
    | otherwise = let updatedVisits = Map.insertWith (+) s 1 numVisits
                      visitsThisNode = Map.findWithDefault 0 s updatedVisits
                      nextGraph = updateGraphAfterVisit s graph visitsThisNode
                      nextPaths = flatMap (visitNode2(nextGraph)(updatedVisits))(children s graph)
                  in filter (isValidPath [] False) (map (\nextPath -> s:nextPath) (filter (\nextPath -> length(nextPath)>0) nextPaths)) 


isValidPath :: [String] -> Bool -> [String] -> Bool
isValidPath _ _ ["end"] = True -- reached the end 
isValidPath pathBehind twiceVisited pathForward
    | isBig(curr) = isValidPath newPathBehind twiceVisited restPathForward
    | twiceVisited && visitingTwice = False -- we can visit  a node twice only once
    | otherwise = isValidPath newPathBehind newtwiceVisited restPathForward
    where curr = head pathForward
          visitingTwice = curr `elem` pathBehind
          newPathBehind = pathBehind ++ [curr]
          newtwiceVisited = twiceVisited || visitingTwice
          restPathForward = tail pathForward

main :: IO ()
main = do
    --putStrLn (show input)
    --putStrLn (show (cave))
    let paths = visitNode cave "start"
    --putStrLn(show paths)
    putStrLn(show(length paths))
    let paths2 = visitNode2 cave Map.empty "start"
    --putStrLn(show paths2)
    putStrLn(show(length paths2))


