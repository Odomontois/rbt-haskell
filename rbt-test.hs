module Main where

import Rbt
import Tree hiding (toList)
import Data.List hiding (insert)
import Control.Applicative

pt m x = let i = insert x m in (i,i)


check Leaf = True
check (Node (RBData c val) left right) 
        | c == Red && (color left == Red || color right == Red) = error $ "Bad color at" ++ show val
        | any (>= val) $ toList left = error $ "Not sorted (left) at" ++ show val 
        | any (< val) $ toList right = error $ "Not sorted (right) at" ++ show val 
        | otherwise = check left && check right

deep::RBT a -> Int
deep t = Rbt.fold (0::Int) (\ _ l r -> 1 + (l `max` r)) t

main = do 
        let a::RBT Int
            a = rbt
            num = 1000
            b = foldr insert a [1..num]
        print $ color b

    --print b
    --print $ check b
    --print $ deep b
    --let c = findSpot 1 $ zipper b
    --print c
    --let d = setTree (Node ( RBData Red 1 ) Leaf Leaf) $ c
    --print d 
    --let e = rollUp d  
    --print e
    --let f = stepUp d
    --print f
