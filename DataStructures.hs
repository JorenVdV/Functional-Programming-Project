module DataStructures where 


-- Type synonyms for String    
type Filename = String
type AttributeName = String
type TargetName = String
type DomainValue = String
type TargetValue = String
type GrapVizTree = String

-- Type synonyms for [String]
type Instance = [String]


-- Data structures
type Attribute = (AttributeName, [DomainValue])

attributeName :: Attribute -> AttributeName
attributeName attribute = fst attribute

domainValues :: Attribute -> [DomainValue]
domainValues attribute = snd attribute

-- Type synonym for [Attribute]
type Set = [Attribute]


data Tree a = 
     Leaf DomainValue TargetValue a
    | Node DomainValue AttributeName [Tree a] a 
    | UncertainLeaf DomainValue [(TargetValue, Float)] a
    deriving (Show, Read, Eq)
    

