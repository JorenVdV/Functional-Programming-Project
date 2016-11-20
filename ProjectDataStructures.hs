module ProjectDataStructures(
    Filename,
    AttributeName,
    DomainValue,
    TargetValue,
    Instance, 
    Attribute,
    DataSet, 
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
data Attribute =  
    Attribute AttributeName DomainValue deriving (Show)

attributeName :: Attribute -> AttributeName
attributeName (Attribute attributeName domainValue ) = attributeName

domainValue :: Attribute -> DomainValue
domainValue (Attribute attributeName domainValue) = domainValue

-- Type synonym for [Attribute]
type DataSet = [Attribute]

-- Type synonym for [DataSet]
type Set = [DataSet]


--data Tree = 
--     Leaf 
--    | Node Attribute [Tree] deriving (Show)

