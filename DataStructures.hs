module ProjectDataStructures(
    Filename,
    AttributeName,
    DomainValue,
    TargetValue,
    Instance
) where 


-- Type synonyms for String    
type Filename = String
type AttributeName = String
type DomainValue = String
type TargetValue = String

-- Type synonyms for [String]
type Instance = [String]


-- Data structures

data Attribute AttributeName DomainValue =  
    Attribute {attributeName::AttributeName, domainValue::DomainValue} deriving (Show)

data Set DataSet =
    Set {dataSet::DataSet} deriving (Show)

data Tree = 
    Leaf 
    | Node (AttributeName, DomainValue) [Tree] 
deriving (Show)

