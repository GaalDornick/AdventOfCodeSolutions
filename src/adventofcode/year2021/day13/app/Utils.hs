module Utils(flatMap) where


flatMap :: ( b -> [a]) -> [b] -> [a]
flatMap mapFunc input = foldl (++) [] (map mapFunc input)