module ProjectUtilityFunctions where

--imports 
import Data.List(maximumBy)
import Data.Ord(comparing)
import ProjectDataStructures(
    Filename,
    AttributeName,
    DomainValue,
    TargetValue,
    Instance,
    Attribute,
    Set,
    attributeName,
    domainValue
    )

-- makes array unique
-- analog to nub implementation by haskell
unique :: (Ord a) => [a] -> [a]
unique list = unique' list []
    where 
        unique' [] _ = []
        unique' (l:ist) seen
            | elem l seen = unique' ist seen
            | otherwise = unique' ist (l:seen)
    
-- returns index of largests element in the row
argmax :: (Ord a) => [a] -> Int
argmax xs =
    fst $  maximumBy cmp indexedxs
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
    head [domainValue x | x <- set , attributeName x == attributename]
