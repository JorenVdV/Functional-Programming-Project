module ID3Tree where

import Data.List
import Data.Ord(comparing)
import DataStructures
import UtilityFunctions
import ReadInput
import ID3Functions.hs

buildTree :: TargetName -> DomainValue -> Set -> Tree Set
buildTree targetname domainvalue set 
    | setpurity set targetname == 0