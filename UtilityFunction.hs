module ProjectUtilityFunctions where

--imports 
import Data.List(maximumBy)
import Data.Ord(comparing)


-- analog to nub implementation by haskell
unique :: (Ord a) => [a] -> [a]
unique list = unique' list []
where unique' [] _ = []
    unique' (l:ist) seen
        | elem l seen = unique ist seen
        | otherwise = l:unique ist (l:seen)
    
-- argmax: returns index of largests element in the row
argmax :: (Ord a) => [a] -> Int
argmax xs =
    fst $  maximumBy cmp indexedxs
    where 
        indexedxs = zip [0..(length xs)-1] xs
        cmp = comparing snd
