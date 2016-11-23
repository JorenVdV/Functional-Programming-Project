module ID3Functions
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


setpurity :: Set -> TargetName -> Float
setpurity set targetname = 
    let 
        targetvalues = getTargetValues set targetname
        grouped = group $ sort targetvalues
        counted = map (\x -> fromIntegral $ length x) grouped
    in
        entropy counted

bestSplit :: Set -> TargetName -> AttributeName
bestSplit set targetname = 
    let 
        attributenames = filter (/=targetname) $ getAttributeNames set
        setpuritycalc = setpurity set targetname
        informationgain = map (\x -> setpuritycalc - (purity set x targetname)) attributenames
        index = argmax informationgain
    in 
        head $ drop index attributenames

splitSet :: Set -> TargetName -> [(DomainValue,Set)]
splitSet set targetname
    | setpurity set targetname == 0 = []
    |otherwise = 
        let 
            attributename = bestSplit set targetname
            domainvalues = unique2 $ getDomainValues set attributename
        in
            map (\x -> (x, createSet set attributename x)) domainvalues

createSet :: Set -> AttributeName -> DomainValue -> Set
createSet set attributename domainvalue =
    let
        attributevals = getDomainValues set attributename
        allattributeNames = filter (/= attributename) $ getAttributeNames set
        alldomainvals = map (getDomainValues set) allattributeNames
        alldomainvalsuntransposed = transpose alldomainvals
        alldomainvalsuntransposedfiltered = map (snd) $ filter (\x -> fst x == domainvalue) $ zip attributevals alldomainvalsuntransposed
        alldomainvalsfiltered = transpose alldomainvalsuntransposedfiltered
    in
        zip allattributeNames alldomainvalsfiltered