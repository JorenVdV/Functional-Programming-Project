module ID3UtilityFunctions
where

import Data.List
import Data.Ord(comparing)
import DataStructures
import UtilityFunctions
import ReadInput

-- a = #positve, b = #negative
entropy :: Float -> Float -> Float
entropy a b 
    | isNaN $ ent a b = 0
    | otherwise = ent a b
    where 
        si1 = a/(a+b)
        si2 = b/(a+b)
        ent a b = - (si1 * logBase 2 si1) - (si2 * logBase 2 si2)

--entropy' :: [Float] -> Float
--entropy' targetcounts
--    |isNaN $ ent' targetcounts = 0
--    | otherwise = ent' targetcounts
--    where
--        total = sum targetcounts
--        s = map (\x -> x/total) targetcounts
--        s2 = map (\x -> -x * (logBase 2 x))
--        ent' targetcounts = foldl1 (+) s2
                

purity :: Set -> AttributeName -> Float
purity set attributename = 
    let
        domainvalues = getDomainValues set attributename
        targetvalues =  getTargetValues set
        numberofvalues = length domainvalues
        groupedvalues = groupBy (\a b -> fst a == fst b) $ sortBy (comparing fst) $ zip domainvalues targetvalues
        -- [(#positve, #negative)]
        countedvalues = map (\x ->
                    (fromIntegral $ length $ filter (\y -> snd y == (snd $ head x)) x,
                    fromIntegral $ length $ filter (\y -> snd y /= (snd $ head x)) x)) 
                    groupedvalues
        -- [(entropy, totaalvandiedomainvaluegeweethe-tanguy)]
        entvalues = map (\x -> (entropy (fst x) (snd x)) * ((fst x)+ (snd x))) countedvalues
        entvaluesadjusted = map (\x -> x / (fromIntegral numberofvalues)) entvalues
    in
        foldl1 (\x element -> x + element) entvaluesadjusted

setpurity :: Set -> Float
setpurity set = 
    let 
        targetvalues = getTargetValues set
        grouped = group $ sort targetvalues
        a = fromIntegral $ length $ head grouped
        b = fromIntegral $ length $ head $ tail grouped
    in
        entropy a b