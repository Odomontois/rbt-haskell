module Main where

import Rbt
import Tree hiding (toList)
import Data.List hiding (insert)
import Control.Applicative
import Data.Monoid

pt m x = let i = insert x m in (i,i)


check Leaf = True
check (Node (RBData c val _ _ ) left right) 
        | c == Red && (color left == Red || color right == Red) = error $ "Bad color at" ++ show val
        | any (>= val) $ toList left = error $ "Not sorted (left) at" ++ show val 
        | any (< val) $ toList right = error $ "Not sorted (right) at" ++ show val 
        | otherwise = check left && check right

deep::RBT a b-> Int
deep t = Rbt.fold (0::Int) (\ _ l r -> 1 + (l `max` r)) t

main = do
        --num <- read <$> getLine::IO Int
        let num = 1234
        let a::RBT Int (Sum Int)
            a = rbt
            b = foldr (uncurry insert) a ( [1,3..num] `zip` map Sum [1,3..num])
        print $ color b
        print $ value b
        print $ aggr b
        print $ aggr $ leftTree b
        print $ aggr $ rightTree b
        print $ aggregate b (-10000) 100000
