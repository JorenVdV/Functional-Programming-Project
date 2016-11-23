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


graphvizTree :: Tree Set -> GrapVizTree
graphvizTree tree =
    let
        gvheader = "digraph id3Tree{\n"
        gvfooter = "}"
    in
        gvheader ++ (makegraphvizTree "\tstart" tree) ++ gvfooter

makegraphvizTree :: String -> Tree Set -> String
makegraphvizTree parent (Node domainvalue attributename subtrees _) =
    let
        current = parent ++ domainvalue
        relation = parent ++ " -> " ++ current ++ " [label=\"" ++ domainvalue ++"\"];\n"
        node = current ++ " [label=\"Node " ++ attributename ++"\"];\n"
        next = foldl1 (++) $ map (\x -> makegraphvizTree current x) subtrees
    in
        relation ++ node ++ next

makegraphvizTree parent (Leaf domainvalue targetvalue _) =
    let
        current = parent ++ domainvalue
        relation = parent ++ " -> " ++ current ++ "[label=\"" ++ domainvalue ++"\"];\n"
        node = current ++ "[label=\"Leaf " ++ targetvalue ++"\"];\n"
    in
        relation ++ node
    
makegraphvizTree parent (UncertainLeaf domainvalue values _) = 
    let
        current = parent ++ domainvalue
        relation = parent ++ " -> " ++ current ++ "[label=" ++ domainvalue ++"];\n"
        node = current ++ "[label=\"UncertainLeaf\"];\n"
        uncertain = foldl1 (++) $ map (mapuncertain current) values
    in
        relation ++ node ++ uncertain

mapuncertain :: String -> (DomainValue, Float) -> String
mapuncertain parent uncertainty =
    let
        uncertainvalue = fst uncertainty
        uncertainlevel = snd uncertainty
        current = parent ++ uncertainvalue
        relation = parent ++ " -> " ++ current ++ " [label=" ++ (show uncertainlevel) ++"];\n"
        node = current ++ " [label=\""++ uncertainvalue ++"\"];\n"
    in
        relation ++ node
    
        

