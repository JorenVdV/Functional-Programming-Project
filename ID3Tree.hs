module ID3Tree where

import Data.List
import Data.Ord(comparing)
import DataStructures
import UtilityFunctions
import ReadWrite
import ID3Functions

buildTree :: TargetName -> DomainValue -> Set -> Tree Set
buildTree targetname domainvalue set 
    | length set == 1 =
        let
            targetvalues = getTargetValues set targetname
            totaltargetvalues = fromIntegral $ length targetvalues
            targetvaluesgrouped = group $ sort $ targetvalues
            targetvaluescounted = map (\x -> (head x, (fromIntegral $ length x)/totaltargetvalues)) targetvaluesgrouped
        in
            UncertainLeaf domainvalue targetvaluescounted set

    | setpurity set targetname == 0 = 
        let
            targetvalue = head $ getTargetValues set targetname
        in
            Leaf domainvalue targetvalue set
        
    | otherwise = 
        let 
            splitset = splitSet set targetname
            attributename = fst splitset
            newsets = snd splitset
        in
            Node domainvalue attributename (map (\x -> buildTree targetname (fst x) (snd x)) newsets) set

id3Tree :: Set -> TargetName -> Tree Set
id3Tree set targetname =
    let
        domainvalue = "root"
    in
        buildTree targetname domainvalue set

