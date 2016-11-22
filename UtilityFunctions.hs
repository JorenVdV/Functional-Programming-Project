module UtilityFunctions where

--imports 
import Data.List
import Data.List(maximumBy)
import Data.Ord(comparing)
import DataStructures

-- makes array unique
-- analog to nub implementation by haskell
unique2 :: (Ord a) => [a] -> [a]
unique2 list = unique' list []
    where 
        unique' [] seen = reverse seen
        unique' (l:ist) seen
            | l `elem` seen = unique' ist seen
            | otherwise = unique' ist (l:seen)

unique :: (Ord a) => [a] -> [a]
unique list = 
    map (head) $ group $ sort list


-- returns index of largests element in the row
argmax :: (Ord a) => [a] -> Int
argmax xs =
    fst $ maximumBy cmp indexedxs
    where 
        indexedxs = zip [0..(length xs)-1] xs
        cmp = comparing snd

getAttributeNames :: Set -> [AttributeName]
getAttributeNames set = 
    [attributeName x | x <- set]

-- retrieves all domainvalues for a given attributename
getDomainValues :: Set -> AttributeName -> [DomainValue]
getDomainValues set attributename =
    -- only take first domainvalue for the attribute name, should be list of 1 element
    head [domainValues x | x <- set , attributeName x == attributename]

getTargetValues :: Set -> TargetName -> [TargetValue]
getTargetValues set targetname = 
    getDomainValues set targetname

