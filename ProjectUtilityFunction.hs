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
    DataSet,
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
        
-- retrieves all possible attributenames, considering rows that have less attributenames
getAttributeNames :: Set -> [AttributeName]
getAttributeNames set = getAttributeNames' set [] 
    where 
        getAttributeNames' [] seen = seen
        getAttributeNames' (s:et) seen =
            getAttributeNames' et $ checkAttributeNames [attributeName x| x<-s] seen
            where
                checkAttributeNames [] seen = seen
                checkAttributeNames (a:ttributes) seen
                    | elem a seen = checkAttributeNames ttributes seen
                    | otherwise = checkAttributeNames ttributes (a:seen)
