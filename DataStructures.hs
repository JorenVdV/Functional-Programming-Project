module DataStructures(
    Filename,
    AttributeName,
    DomainValue,
    TargetValue,
    Instance, 
    Attribute,
    Set,
    attributeName,
    domainValue
) where 


-- Type synonyms for String    
type Filename = String
type AttributeName = String
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

domainValue :: Attribute -> [DomainValue]
domainValue attribute = snd attribute

-- Type synonym for [Attribute]
type Set = [Attribute]


--data Tree = 
--     Leaf 
--    | Node Attribute [Tree] deriving (Show)

