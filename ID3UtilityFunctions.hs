module ID3UtilityFunctions
where

import Data.List
import Data.Ord(comparing)
import DataStructures
import UtilityFunctions
import ReadInput


entropysingle :: Float -> Float -> Float
entropysingle elementsforval totalelements
    | isNaN $ ent elementsforval totalelements = 0
    | otherwise = ent elementsforval  totalelements
    where 
        px = elementsforval / totalelements
        ent elementsforval totalelements = (px * logBase 2 px)

entropy :: [Float] -> Float
entropy list =
    let
        listtotal = sum list
    in
     -(sum $ map (\x -> entropysingle x listtotal) list)
           

purity :: Set -> AttributeName -> TargetName-> Float
purity set attributename targetname= 
    let
        domainvalues = getDomainValues set attributename
        targetvalues =  getTargetValues set targetname
        numberofvalues = length domainvalues
        groupedvaluesbydomain = groupBy (\a b -> fst a == fst b) $ sortBy (comparing fst) $ zip domainvalues targetvalues
        groupedvaluesbydomainandtarget = map (groupBy (\a b -> snd a == snd b)) $ map (sortBy (comparing snd)) groupedvaluesbydomain
        -- [[countpertargevalue]]
        countedvalues = map (\x -> map (\y ->fromIntegral $ length y) x) groupedvaluesbydomainandtarget

        -- [(entropy, totaalvandiedomainvaluegeweethe-tanguy)]
        entvalues = map (\x -> (entropy x) * (sum x) ) countedvalues
        --entvalues = map (\x -> (entropy (fst x) (snd x)) * ((fst x)+ (snd x))) countedvalues
        entvaluesadjusted = map (\x -> x / (fromIntegral numberofvalues)) entvalues
    in
        foldl1 (\x element -> x + element) entvaluesadjusted


setpurity :: Set -> TargetValue -> Float
setpurity set targetvalue = 
    let 
        targetvalues = getTargetValues set targetvalue
        grouped = group $ sort targetvalues
        counted = map (\x -> fromIntegral $ length x) grouped
    in
        entropy counted