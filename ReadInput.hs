module ReadInput where 

import System.IO
import Data.Char
import Data.List.Split
import Data.List
import DataStructures

readCsv :: Filename -> IO Set
readCsv filename = do
    contents <- readFile filename
    return $ parseCsv contents

parseCsv :: String -> Set
parseCsv contents = 
    let contentlines = lines contents
        header = createInstance $ head contentlines
        instances = map createInstance $ tail contentlines
    in 
        createAttributes header instances

createInstance :: String -> Instance
createInstance instanceStr =
    splitOn "," instanceStr

createAttributes :: [String] -> [Instance] -> [Attribute]
createAttributes headerStr instances =
    let 
        transposedInstances = transpose instances
    in
        zip headerStr transposedInstances

