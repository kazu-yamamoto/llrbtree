{-|
  Skew Heap

  - the fun of programming
-}

module Data.Heap.Skew where

import Prelude hiding (minimum)

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

null :: Tree t -> Bool
null Leaf         = True
null (Node _ _ _) = False

empty :: Tree a
empty = Leaf

singleton :: a -> Tree a
singleton x = Node Leaf x Leaf

minimum :: Tree t -> t
minimum (Node _ x _) = x
minimum _            = error "minimum"

deleteMin :: Ord a => Tree a -> Tree a
deleteMin (Node l _ r) = merge l r
deleteMin _            = error "deleteMin"

insert :: Ord a => a -> Tree a -> Tree a
insert x t = merge (singleton x) t

merge :: Ord a => Tree a -> Tree a -> Tree a
merge t1 Leaf = t1
merge Leaf t2 = t2
merge t1 t2
  | minimum t1 <= minimum t2 = join t1 t2
  | otherwise                = join t2 t1

join :: Ord a => Tree a -> Tree a -> Tree a
join (Node l x r) t = Node r x (merge l t)
join _ _ = error "join"
