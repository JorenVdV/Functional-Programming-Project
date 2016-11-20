module ProjectReadInput where 

import System.IO
import Data.Char
import Data.List.Split
import ProjectDataStructures(
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
    )

readCsv :: Filename -> IO Set
readCsv filename = do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    hClose handle
    parsed <- parseCsv contents
    return parsed

parseCsv :: String -> Set
parseCsv contents = 
    let contentlines = lines contents
    in 
        createAttributes (createInstance (head contentlines)) (map createInstance (tail contentlines))

createInstance :: String -> Instance
createInstance line =
    splitOn "," line

createAttributes :: [String] -> [Instance] -> [Attribute]
createAttributes attributes datasets =
    [createAttributes' attributes y | y <- datasets]
    where 
        createAttributes' attributes dataset= 
            [Attribute (fst x) (snd x) | x <- (zip attributes instances)]
