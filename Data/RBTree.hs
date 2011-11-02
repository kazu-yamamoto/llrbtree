{-# LANGUAGE CPP #-}

module Data.RBTree (
    RBTree(..)
  , Color(..)
  , empty
  , insert
  , fromList
  , toList
  , member
  , delete
  , deleteMin
  , deleteMax
  , union
  , intersection
  , difference
  , join
  , merge
  , split
  , valid
  , showTree
  , printTree
  ) where

import Data.List (foldl')
import Data.RBTree.Internal
#if METHOD == 1
import Data.RBTree.Original
#else
import Data.RBTree.LL
#endif
import Prelude hiding (minimum, maximum)

----------------------------------------------------------------

fromList :: Ord a => [a] -> RBTree a
fromList = foldl' (flip insert) empty

----------------------------------------------------------------

member :: Ord a => a -> RBTree a -> Bool
member _ Leaf = False
member x (Node _ _ l y r) = case compare x y of
    LT -> member x l
    GT -> member x r
    EQ -> True

----------------------------------------------------------------

{-
  Each element of t1 < g.
  Each element of t2 > g.
-}

join :: Ord a => RBTree a -> a -> RBTree a -> RBTree a
join Leaf g t2 = insert g t2
join t1 g Leaf = insert g t1
join t1 g t2 = case compare h1 h2 of
    LT -> turnB $ joinLT t1 g t2 h1
    GT -> turnB $ joinGT t1 g t2 h2
    EQ -> Node B (h1+1) t1 g t2
  where
    h1 = height t1
    h2 = height t2

-- The root of result must be red.
joinLT :: Ord a => RBTree a -> a -> RBTree a -> BlackHeight -> RBTree a
joinLT t1 g t2@(Node c h l x r) h1
  | h == h1   = Node R (h+1) t1 g t2
  | otherwise = balanceL c h (joinLT t1 g l h1) x r
joinLT _ _ _ _ = error "joinLT"

-- The root of result must be red.
joinGT :: Ord a => RBTree a -> a -> RBTree a -> BlackHeight -> RBTree a
joinGT t1@(Node c h l x r) g t2 h2
  | h == h2   = Node R (h+1) t1 g t2
  | otherwise = balanceR c h l x (joinGT r g t2 h2)
joinGT _ _ _ _ = error "joinGT"

----------------------------------------------------------------

merge :: Ord a => RBTree a -> RBTree a -> RBTree a
merge Leaf t2 = t2
merge t1 Leaf = t1
merge t1 t2 = case compare h1 h2 of
    LT -> turnB $ mergeLT t1 t2 h1
    GT -> turnB $ mergeGT t1 t2 h2
    EQ -> turnB $ mergeEQ t1 t2
  where
    h1 = height t1
    h2 = height t2

mergeLT :: Ord a => RBTree a -> RBTree a -> BlackHeight -> RBTree a
mergeLT t1 t2@(Node c h l x r) h1
  | h == h1   = mergeEQ t1 t2
  | otherwise = balanceL c h (mergeLT t1 l h1) x r
mergeLT _ _ _ = error "mergeLT"

mergeGT :: Ord a => RBTree a -> RBTree a -> BlackHeight -> RBTree a
mergeGT t1@(Node c h l x r) t2 h2
  | h == h2   = mergeEQ t1 t2
  | otherwise = balanceR c h l x (mergeGT r t2 h2)
mergeGT _ _ _ = error "mergeGT"

{-
  Merging two trees whose heights are the same.
  The root must be either
     a red with height + 1
  for
     a black with height
-}

mergeEQ :: Ord a => RBTree a -> RBTree a -> RBTree a
mergeEQ Leaf Leaf = Leaf
mergeEQ t1@(Node _ h l x r) t2
  | h == h2'  = Node R (h+1) t1 m t2'
  | isRed l   = Node R (h+1) (turnB l) x (Node B h r m t2')
  | isRed r   = Node B h (Node R h l x rl) rx (Node R h rr m t2')
  | otherwise = Node B h (turnR t1) m t2'
  where
    m  = minimum t2
    t2' = deleteMin t2
    h2' = height t2'
    Node R _ rl rx rr = r
mergeEQ _ _ = error "mergeEQ"

{-
mergeEQ :: Ord a => RBTree a -> RBTree a -> RBTree a
mergeEQ Leaf Leaf = Leaf
mergeEQ t1@(Node _ h l x r) t2
  | h == h2'  = Node R (h+1) t1 m t2'
  | isRed l   = Node R (h+1) (turnB l) x (Node B h r m t2')
  | otherwise = Node B h (turnR t1) m t2'
  where
    m  = minimum t2
    t2' = deleteMin t2
    h2' = height t2'
mergeEQ _ _ = error "mergeEQ"
-}

----------------------------------------------------------------

split :: Ord a => a -> RBTree a -> (RBTree a, RBTree a)
split _ Leaf = (Leaf,Leaf)
split kx (Node _ _ l x r) = case compare kx x of
    LT -> (lt, join gt x (turnB' r)) where (lt,gt) = split kx l
    GT -> (join (turnB' l) x lt, gt) where (lt,gt) = split kx r
    EQ -> (turnB' l, turnB' r)

{- LL XXX why this?
split :: (Show a, Ord a) => a -> RBTree a -> (RBTree a, RBTree a)
split _ Leaf = (Leaf,Leaf)
split kx (Node _ _ l x r) = case compare kx x of
    LT -> (lt, join gt x r) where (lt,gt) = split kx l
    GT -> (join l x lt, gt) where (lt,gt) = split kx r
    EQ -> (turnB' l, r)
-}

----------------------------------------------------------------

union :: Ord a => RBTree a -> RBTree a -> RBTree a
union t1 Leaf = t1
union Leaf t2 = t2
union t1 (Node _ _ l x r) = join (union l' (turnB' l)) x (union r' (turnB' r))
  where
    (l',r') = split x t1

{- LL
union :: Ord a => RBTree a -> RBTree a -> RBTree a
union t1 Leaf = t1
union Leaf t2 = t2
union t1 (Node _ _ l x r) = join (union l' (turnB' l)) x (union r' r)
  where
    (l',r') = split x t1
-}

intersection :: Ord a => RBTree a -> RBTree a -> RBTree a
intersection Leaf _ = Leaf
intersection _ Leaf = Leaf
intersection t1 (Node _ _ l x r)
  | member x t1 = join (intersection l' l) x (intersection r' r)
  | otherwise   = merge (intersection l' l) (intersection r' r)
  where
    (l',r') = split x t1

difference :: Ord a => RBTree a -> RBTree a -> RBTree a
difference Leaf _  = Leaf
difference t1 Leaf = t1
difference t1 (Node _ _ l x r) = merge (difference l' l) (difference r' r)
  where
    (l',r') = split x t1
