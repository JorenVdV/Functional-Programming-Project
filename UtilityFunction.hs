module ProjectUtilityFunctions where

--imports 
import Data.List(maximumBy)
import Data.Ord(comparing)
import ProjectDataStructures(Filename,AttributeName,DomainValue,TargetValue,Instance,Attribute,Set)

-- makes array unique
-- analog to nub implementation by haskell
unique :: (Ord a) => [a] -> [a]
unique list = unique' list []
    where 
        unique' [] _ = []
        unique' (l:ist) seen
            | elem l seen = unique ist seen
            | otherwise = l:unique ist (l:seen)
    
-- returns index of largests element in the row
argmax :: (Ord a) => [a] -> Int
argmax xs =
    fst $  maximumBy cmp indexedxs
    where 
        indexedxs = zip [0..(length xs)-1] xs
        cmp = comparing snd
        
-- retrieves all possible attributenames, considering rows that have less attributenames
getAttributeNames :: Set -> [AttributeNames]
getAttributeNames set = getAttributeNames' set [] 
    where 
        getAttributeNames' [] seen = seen
        getAttributeNames' (s:et) seen =
            getAttributeNames' et $ checkAttributeNames [x-> attributename x| x<-s] seen
            where
                checkAttributeNames [] seen = seen
                checkAttributeNames (a:ttributes) seen
                    | elem a seen = seen
                    | otherwise = a:seen

