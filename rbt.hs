module Rbt where

import Tree
import Data.Monoid

data NodeColor = Black | Red deriving (Show , Eq)

data RBData a b= RBData{ rbtColor::NodeColor, rbtValue::a , rbtCalc::b, rbtAggr::b} deriving Show

type RBT a b= Tree (RBData a b) 
type RBTZ a b = Zipper (RBData a b)

color (Node (RBData color _ _ _) _ _) = color
color Leaf = Black

aggr Leaf = mempty
aggr (Node (RBData _ _ _ aggr) _ _ ) = aggr

value (Node (RBData _ value _ _) _ _) = value

toList = map rbtValue . Tree.toList

dad = stepUp
grand = dad . dad 
bro zipper = case direction zipper of 
    Tree.Left -> goRight $ dad zipper
    Tree.Right -> goLeft $ dad zipper
uncle = bro . dad 
colorZ = color . treeZ
goAnother z = case direction z  of Tree.Left  -> goRight
                                   Tree.Right -> goLeft   

setColor::NodeColor->RBTZ a b-> RBTZ a b
setColor c = modTree $ modValue changeColor where
    changeColor (RBData _ x y z) = RBData c x y z 

rbt::(Ord a, Monoid b)=> RBT a b
rbt = Leaf

insert::(Ord a, Monoid b)=> a -> b -> RBT a b-> RBT a b
insert x m rbt = 
        let     branchAggr Nothing = mempty
                branchAggr (Just branch) = rbtAggr branch

                transform that Nothing Nothing = RBData (rbtColor that) (rbtValue that) (rbtCalc that) (rbtCalc that)
                transform that left right      = RBData (rbtColor that) (rbtValue that) (rbtCalc that) (rbtCalc that <> branchAggr left <> branchAggr right)

                start = zipperTF rbt transform 
                findSpot z = findSpotT $ treeZ z where 
                        findSpotT Leaf = z
                        findSpotT (Node (RBData _ y _ _) _ _ ) 
                                | x < y     = findSpot $ goLeft z 
                                | otherwise = findSpot $ goRight z 
                newNode = Node ( RBData Red x m m) Leaf Leaf 
                inserted = setTree newNode $ findSpot start 
                fix z   | isTop z                                               = treeZ z  --black root is ok 
                        | (colorZ $ dad z ) == Black                            = rollUp z --black dad is pretty ok
                        | isTop $ dad z                                         = treeZ $ setColor Black $ dad z 
                        | (colorZ $ dad z) == Red && (colorZ $ uncle z) == Red  = let 
                                dad' = setColor Black $ dad z
                                uncle' = setColor Black $ bro dad'
                                grand' = setColor Red $ dad uncle'
                                in fix $ grand'
                        | direction z == (direction $ prevZ z)                    = let
                                dad' = setColor Black $ rotate $ dad z
                                dad''= dad $ setColor Red $ goAnother z  dad'
                                in fix dad''
                        | otherwise                                              = fix $ goAnother z $ rotate z 
        in fix inserted    

fold::b->(a->b->b->b)->RBT a c->b
fold s f Leaf = s
fold s f (Node rbx left right ) = f (rbtValue rbx) (fold s f left) (fold s f right) 












