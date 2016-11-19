module ProjectDataStructures(
    Filename,
    AttributeName,
    DomainValue,
    TargetValue,
    Instance, 
    Attribute, 
    Set
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
    Attribute {attributeName::AttributeName, domainValue::DomainValue} deriving (Show)

data DataSet = DataSet [Attribute] deriving (Show)

data Set = Set [DataSet] deriving (Show)

--data Tree = 
--     Leaf 
--    | Node Attribute [Tree] deriving (Show)

