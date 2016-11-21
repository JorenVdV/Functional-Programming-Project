module ID3UtilityFunctions (
    entropy
    ) 
where

import Data.List
import DataStructures
import UtilityFunctions

entropy :: Float -> Float -> Float
entropy a b 
    | isNaN $ ent a b = 0
    | otherwise = ent a b
    where 
        ent a b = 
            - (si1 * logBase 2 si1) - (si2 * logBase 2 si2)
            where 
                si1 = a/b
                si2 = (b-a)/b
                

purity :: Set -> AttributeName -> Float
purity set attributename = 
    let
        domainvalues = getDomainValues set attributename
        targetvalues =  getTargetValues set
        numberofvalues = length domainvalues
        groupedvalues = groupBy (\a b -> fst a == fst b) $ zip domainvalues targetvalues
        
        countedvalues = map (\x -> ((fst $ head x),
                    (fromIntegral $ length $ filter (\y -> snd y == (snd $ head x)) x,
                    fromIntegral $ length $ filter (\y -> snd y /= (snd $ head x)) x))) groupedvalues

        entvalues = map (\x -> entropy (fst $ snd x) ((fst $ snd x) + (snd $ snd x))) countedvalues
        igcoeffs = map (\x -> (((fst $ snd x) + (snd $ snd x))) `div` numberofvalues) countedvalues
    in
       0

setpurity :: Set -> Float
setpurity set = 
    let 
        targetvalues = getTargetValues set
        numberofvalues = length targetvalues
        grouped = group targetvalues
    in
        0

        


