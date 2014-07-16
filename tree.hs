module Tree where

import Prelude hiding (Left,Right)

data Tree a = Node a (Tree a) (Tree a) | Leaf deriving Show
data Direction = Left | Right deriving (Show, Eq)
data Zipper a =  Zipper{direction:: Direction, treeZ::Tree a, modified::Bool, prev::Zipper a} 
                |Top  {                        treeZ::Tree a, modified::Bool } deriving Show

modValue:: (a -> a) ->Tree a->Tree a
modValue _ Leaf = Leaf
modValue f (Node x left right) = Node (f x) left right

zipper::Tree a -> Zipper a
zipper tree = Top tree False

isTop (Top _ _) = True
isTop (Zipper _ _ _ _)  = False

goLeft::Zipper a -> Zipper a
goLeft current = case (treeZ current) of  Node _ left _ -> Zipper Left left False current
                                          Leaf          -> error "Couldn't go anywhere from leaf"

goRight::Zipper a -> Zipper a
goRight current = case (treeZ current) of  Node _ _ right -> Zipper Right right False current
                                           Leaf           -> error "Couldn't go anywhere from leaf"

replace (Node x left right) modified direction = case direction of 
    Left -> Node x modified right
    Right -> Node x left modified

stepUp::Zipper a -> Zipper a
stepUp (Top _  _) = error "Couldn't get higher than top"
stepUp (Zipper _ _ False prev) = prev
stepUp (Zipper direction tree True zipper) = let replaced = replace (treeZ zipper) tree direction in
    case zipper of Zipper dir _ modif prev -> Zipper dir replaced True prev
                   Top    _ modif          -> Top    replaced True

rollUp::Zipper a -> Tree a
rollUp (Top tree _ ) = tree
rollUp z@(Zipper _ _ _ _) = rollUp (stepUp z)

modTree::(Tree a -> Tree a) -> Zipper a -> Zipper a
modTree transform (Top node _ ) =  Top (transform node) True
modTree transform (Zipper direction node _ prev) = Zipper direction (transform node) True prev

setTree tree = modTree (const tree)
setValue x = let transform (Node _ l r) = Node x l r in modTree transform

rotate::Zipper a -> Zipper a
rotate zipper@(Top _ _ ) = zipper
rotate (Zipper _ Leaf _ _) = error "Can not rotate Zipper"
rotate (Zipper Left (Node c left center) modif zipper) =
    let (Node p _ right ) = treeZ zipper in
    let modParent = Node p center right in 
    let modChild = Node c left modParent in 
    setTree modChild zipper
rotate (Zipper Right (Node c center right) modif zipper) =
    let (Node p left _) = treeZ zipper in
    let modParent = Node p left center in 
    let modChild = Node c modParent right in
    setTree modChild zipper

fromList::[a] -> Tree a
fromList [] = Leaf
fromList [x] = Node x Leaf Leaf
fromList (x:others) = Node x Leaf $ fromList others

toList::Tree a -> [a]
toList Leaf = []
toList (Node x t1 t2) = toList t1 ++ x:toList t2

invert::Tree a -> Tree a
invert Leaf = Leaf
invert (Node x t1 t2) = Node x (invert t2) (invert t1)





