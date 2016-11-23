module Main where

import DataStructures
import UtilityFunctions
import ReadWrite
import ID3Functions
import ID3Tree

main :: IO()
main = do
    dataset <- readCsv "weather.csv"
    let tree = id3Tree dataset "Outlook"
    let dottree = graphvizTree tree
    writeFile  "weather.dot" dottree
    return ()