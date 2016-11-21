module ProjectReadInput where 

import System.IO
import Data.Char
import Data.List.Split
import Data.List
import ProjectDataStructures(
    Filename,
    AttributeName,
    DomainValue,
    TargetValue,
    Instance,
    Attribute,
    Set,
    attributeName,
    domainValue
    )

readCsv :: Filename -> IO Set
readCsv filename = do
    contents <- readFile filename
    return $ parseCsv contents

parseCsv :: String -> Set
parseCsv contents = 
    let contentlines = lines contents
    in 
        createAttributes (createInstance (head contentlines)) (map createInstance (tail contentlines))

createInstance :: String -> Instance
createInstance instanceStr =
    splitOn "," instanceStr

createAttributes :: [String] -> [Instance] -> [Attribute]
createAttributes headerStr instances =
    let transposedInstances = transpose instances
        zippedInstances = zip headerStr transposedInstances
    in
        zippedInstances

