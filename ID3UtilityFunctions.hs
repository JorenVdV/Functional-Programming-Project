module ID3UtilityFunctions (
    entropy
    ) 
where

import DataStructures(
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

entropy :: Float -> Float -> Float
entropy a b 
    | isNaN $ ent a b = 0
    | otherwise = ent a b
    where 
        ent a b =
            let 
                si1 = a/b
                si2 = (b-a)/b
            in 
                - (si1 * logBase 2 si1) - (si2 * logBase 2 si2)
        


