module Rbt where

import Tree

data NodeColor = Black | Red deriving (Show , Eq)

data RBData a = RBData{ rbtColor::NodeColor, rbtValue::a} deriving Show

type RBT a = Tree (RBData a)
type RBTZ a = Zipper (RBData a)

color (Node (RBData color _) _ _) = color
color Leaf = Black

value (Node (RBData _ value ) _ _) = value

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

setColor::NodeColor->RBTZ a -> RBTZ a
setColor c = modTree $ modValue changeColor where
    changeColor (RBData _ x) = RBData c x   

rbt::Ord a => RBT a
rbt = Leaf

findSpot::Ord a => a -> Zipper (RBData a) -> Zipper (RBData a)
findSpot x z = findSpotT $ treeZ z where
    findSpotT Leaf = z
    findSpotT (Node (RBData _ y ) _ _ ) |x < y     = findSpot x $ goLeft z 
                                        |otherwise = findSpot x $ goRight z

insert::Ord a => a -> RBT a -> RBT a
insert x rbt = 
        let start = zipper rbt in
        let findSpot z = findSpotT $ treeZ z where 
                findSpotT Leaf = z
                findSpotT (Node (RBData _ y ) _ _ ) | x < y     = findSpot $ goLeft z 
                                                    | otherwise = findSpot $ goRight z in
    let newNode = Node ( RBData Red x  ) Leaf Leaf in
    let inserted = setTree newNode $ findSpot start in
    let fix z   | isTop z                                               = treeZ z  --black root is ok 
                | (colorZ $ dad z ) == Black                            = rollUp z --black dad is pretty ok
                | isTop $ dad z                                         = treeZ $ setColor Black $ dad z 
                | (colorZ $ dad z) == Red && (colorZ $ uncle z) == Red  = let 
                        dad' = setColor Black $ dad z
                        uncle' = setColor Black $ bro dad'
                        grand' = setColor Red $ dad uncle'
                        in fix $ grand'
                | direction z == (direction $ prev z)                    = let
                        dad' = setColor Black $ rotate $ dad z
                        dad''= dad $ setColor Red $ goAnother z  dad'
                        in fix dad''
                | otherwise                                              = fix $ goAnother z $ rotate z 
    in fix inserted    

fold::b->(a->b->b->b)->RBT a->b
fold s f Leaf = s
fold s f (Node rbx left right ) = f (rbtValue rbx) (fold s f left) (fold s f right) 












