module DataStructures where 


-- Type synonyms for String    
type Filename = String
type AttributeName = String
type TargetName = String
type DomainValue = String
type TargetValue = String

-- Type synonyms for [String]
type Instance = [String]


-- Data structures
type Attribute = (AttributeName, [DomainValue])
--data Attribute =  
--    Attribute AttributeName [DomainValue] deriving (Show)

attributeName :: Attribute -> AttributeName
attributeName attribute = fst attribute

domainValues :: Attribute -> [DomainValue]
domainValues attribute = snd attribute

-- Type synonym for [Attribute]
type Set = [Attribute]


data Tree a = 
     Leaf TargetValue
    | Node a DomainValue [Tree a] deriving (Show, Read, Eq)

