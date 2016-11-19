module ProjectUtilityFunctions where

--imports 
import Data.List(maximumBy)
import Data.Ord(comparing)

--unique :: (Ord a) => [a] -> [a]
--unique list =
    
-- argmax: returns index of largests element in the row
argmax :: (Ord a) => [a] -> Int
argmax xs =
    fst $  maximumBy cmp indexedxs
    where 
        indexedxs = zip [0..(length xs)-1] xs
        cmp = comparing snd
