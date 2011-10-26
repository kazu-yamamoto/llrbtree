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
  , valid
  ) where

import Data.List (foldl')
import Data.RBTree.Internal

----------------------------------------------------------------

valid :: RBTree a -> Bool
valid = isBalanced

----------------------------------------------------------------

fromList :: Ord a => [a] -> RBTree a
fromList = foldl' (flip insert) empty

----------------------------------------------------------------
-- Chris Okasaki
--

insert :: Ord a => a -> RBTree a -> RBTree a
insert a b = Fork B d e f
  where
    Fork _ d e f = ins a b
    ins x Leaf = Fork R Leaf x Leaf
    ins x t@(Fork c l y r) = case compare x y of
        LT -> balanceL c (ins x l) y r
        GT -> balanceR c l y (ins x r)
        EQ -> t

balanceL :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balanceL B (Fork R (Fork R a x b) y c) z d =
    Fork R (Fork B a x b) y (Fork B c z d)
balanceL B (Fork R a x (Fork R b y c)) z d =
    Fork R (Fork B a x b) y (Fork B c z d)
balanceL k a x b = Fork k a x b

balanceR :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balanceR B a x (Fork R b y (Fork R c z d)) =
    Fork R (Fork B a x b) y (Fork B c z d)
balanceR B a x (Fork R (Fork R b y c) z d) =
    Fork R (Fork B a x b) y (Fork B c z d)
balanceR k a x b = Fork k a x b

----------------------------------------------------------------

type RBTreeBDel a = (RBTree a, Bool)

unbalancedL :: Color -> RBTree a -> a -> RBTree a -> RBTreeBDel a
unbalancedL c (Fork B t1 x1 t2) x2 t3 = (balanceL B (Fork R t1 x1 t2) x2 t3, c == B)
unbalancedL B (Fork R t1 x1 (Fork B t2 x2 t3)) x3 t4 = (Fork B t1 x1 (balanceL B (Fork R t2 x2 t3) x3 t4), False)
unbalancedL _ _ _ _ = error "unbalancedL"

-- The left tree lacks one Black node
unbalancedR :: Color -> RBTree a -> a -> RBTree a -> (RBTree a, Bool)
-- Decreasing one Black node in the right
unbalancedR c t1 x1 (Fork B t2 x2 t3) = (balanceR B t1 x1 (Fork R t2 x2 t3), c == B)
-- Takeing one Red node from the right and adding it to the right as Black
unbalancedR B t1 x1 (Fork R (Fork B t2 x2 t3) x3 t4) = (Fork B (balanceR B t1 x1 (Fork R t2 x2 t3)) x3 t4, False)
unbalancedR _ _ _ _ = error "unbalancedR"

----------------------------------------------------------------

deleteMin :: RBTree a -> RBTree a
deleteMin t = t'
  where
    ((t', _), _) = deleteMin' t

deleteMin' :: RBTree a -> (RBTreeBDel a, a)
deleteMin' Leaf                           = error "deleteMin'"
deleteMin' (Fork B Leaf x Leaf)           = ((Leaf, True), x)
deleteMin' (Fork B Leaf x (Fork R l y r)) = ((Fork B l y r, False), x)
deleteMin' (Fork R Leaf x r)              = ((r, False), x)
deleteMin' (Fork c l x r)                 = if d then (tD, m) else (tD', m)
  where
    ((l',d),m) = deleteMin' l
    tD  = unbalancedR c l' x r
    tD' = (Fork c l' x r, False)

----------------------------------------------------------------

blackify :: RBTree a -> RBTreeBDel a
blackify s@(Fork R _ _ _) = (turnB s, False)
blackify s                = (s, True)

delete :: Ord a => a -> RBTree a -> RBTree a
delete x s = s'
  where
    (s',_) = delete' x s
    
delete' :: Ord a => a -> RBTree a -> RBTreeBDel a
delete' _ Leaf = (Leaf, False)
delete' x (Fork c l y r) = case compare x y of
    LT -> let (l',d) = delete' x l
              t = Fork c l' y r
          in if d then unbalancedR c l' y r else (t, False)
    GT -> let (r',d) = delete' x r
              t = Fork c l y r'
          in if d then unbalancedL c l y r' else (t, False)
    EQ -> case r of
        Leaf -> if c == B then blackify l else (l, False)
        _ -> let ((r',d),m) = deleteMin' r
                 t = Fork c l m r'
             in if d then unbalancedL c l m r' else (t, False)
