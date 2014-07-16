module Tree where

import Prelude hiding (Left,Right)

data Tree a = Node{ nodeVal::a, leftTree::Tree a, rightTree:: Tree a} | Leaf deriving Show
data Direction = Left | Right deriving (Show, Eq)
type TF a = a -> Maybe a -> Maybe a -> a
data Zipper a =  Zipper{direction:: Direction, treeZ::Tree a, modifiedZ::Bool, prevZ::Zipper a, transformZ::TF a} 
                |Top  {                        treeZ::Tree a, modifiedZ::Bool                ,  transformZ::TF a}

modValue:: (a -> a) ->Tree a->Tree a
modValue _ Leaf = Leaf
modValue f (Node x left right) = Node (f x) left right

treeVal (Node x _ _)  = Just x
treeVal Leaf        = Nothing

constTF = const . const 

zipper::Tree a -> Zipper a
zipper tree = Top tree False constTF
zipperTF tree tf = Top tree False tf

isTop (Top _ _ _) = True
isTop (Zipper _ _ _ _ _)  = False

goLeft::Zipper a -> Zipper a
goLeft current = case (treeZ current) of  Node _ left _ -> Zipper Left left False current (transformZ current)
                                          Leaf          -> error "Couldn't go anywhere from leaf"

goRight::Zipper a -> Zipper a
goRight current = case (treeZ current) of  Node _ _ right -> Zipper Right right False current (transformZ current)
                                           Leaf           -> error "Couldn't go anywhere from leaf"

replace (Node x left right) modified direction transform = 
        let     (left', right') = case direction of 
                        Left -> (modified, right)
                        Right -> (left, modified)
                x' = transform x (treeVal left') (treeVal right')    
        in Node x' left' right'

stepUp::Zipper a -> Zipper a
stepUp (Top _ _ _) = error "Couldn't get higher than top"
stepUp (Zipper _ _ False prev _) = prev
stepUp (Zipper direction tree True zipper tf) = let replaced = replace (treeZ zipper) tree direction tf in
    case zipper of Zipper dir _ modif prev tf -> Zipper dir replaced True prev tf
                   Top    _ modif tf          -> Top    replaced True tf

rollUp::Zipper a -> Tree a
rollUp (Top tree _ _) = tree
rollUp z@(Zipper _ _ _ _ _) = rollUp (stepUp z)

recalc::TF a -> Tree a -> Tree a
recalc _ Leaf = Leaf
recalc tf (Node x left right) = Node (tf x (treeVal left) (treeVal right)) left right

modTree::(Tree a -> Tree a) -> Zipper a -> Zipper a
modTree transform (Top node _ tf) =  Top (recalc tf $ transform node) True tf
modTree transform (Zipper direction node _ prev tf) = Zipper direction (recalc tf $ transform node) True prev tf

setTree tree = modTree (const tree)
setValue x = let transform (Node _ l r) = Node x l r in modTree transform

rotate::Zipper a -> Zipper a
rotate zipper@(Top _ _ _ ) = zipper
rotate (Zipper _ Leaf _ _ _ ) = error "Can not rotate Zipper"
rotate (Zipper Left (Node c left center) modif zipper _) =
    let (Node p _ right ) = treeZ zipper in
    let modParent = Node p center right in 
    let modChild = Node c left modParent in 
    setTree modChild zipper
rotate (Zipper Right (Node c center right) modif zipper _) =
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





