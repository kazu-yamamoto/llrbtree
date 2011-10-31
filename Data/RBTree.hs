module Data.RBTree (
    RBTree(..)
  , Color(..)
  , empty
  , insert
  , fromList
  , toList
  , member
{-
  , delete
  , deleteMin
-}
  , valid
  ) where

import Data.List (foldl')
import Data.RBTree.Internal

----------------------------------------------------------------

valid :: RBTree a -> Bool
valid t = isBalanced t && blackHeight t

----------------------------------------------------------------

fromList :: Ord a => [a] -> RBTree a
fromList = foldl' (flip insert) empty

----------------------------------------------------------------
-- Chris Okasaki
--

insert :: Ord a => a -> RBTree a -> RBTree a
insert kx t = turnB (insert' kx t)

insert' :: Ord a => a -> RBTree a -> RBTree a
insert' kx Leaf = Node R 1 Leaf kx Leaf
insert' kx s@(Node c h l x r) = case compare kx x of
    LT -> balanceL c h (insert' kx l) x r
    GT -> balanceR c h l x (insert' kx r)
    EQ -> s

balanceL :: Color -> BlackHeight -> RBTree a -> a -> RBTree a -> RBTree a
balanceL B h (Node R _ (Node R _ a x b) y c) z d =
    Node R (h+1) (Node B h a x b) y (Node B h c z d)
balanceL B h (Node R _ a x (Node R _ b y c)) z d =
    Node R (h+1) (Node B h a x b) y (Node B h c z d)
balanceL k h l x r = Node k h l x r

balanceR :: Color -> BlackHeight -> RBTree a -> a -> RBTree a -> RBTree a
balanceR B h a x (Node R _ b y (Node R _ c z d)) =
    Node R (h+1) (Node B h a x b) y (Node B h c z d)
balanceR B h a x (Node R _ (Node R _ b y c) z d) =
    Node R (h+1) (Node B h a x b) y (Node B h c z d)
balanceR k h l x r = Node k h l x r

{-XXX
----------------------------------------------------------------

type RBTreeBDel a = (RBTree a, Bool)

unbalancedL :: Color -> RBTree a -> a -> RBTree a -> RBTreeBDel a
unbalancedL c (Node B t1 x1 t2) x2 t3 = (balanceL B (Node R t1 x1 t2) x2 t3, c == B)
unbalancedL B (Node R t1 x1 (Node B t2 x2 t3)) x3 t4 = (Node B t1 x1 (balanceL B (Node R t2 x2 t3) x3 t4), False)
unbalancedL _ _ _ _ = error "unbalancedL"

-- The left tree lacks one Black node
unbalancedR :: Color -> RBTree a -> a -> RBTree a -> (RBTree a, Bool)
-- Decreasing one Black node in the right
unbalancedR c t1 x1 (Node B t2 x2 t3) = (balanceR B t1 x1 (Node R t2 x2 t3), c == B)
-- Taking one Red node from the right and adding it to the right as Black
unbalancedR B t1 x1 (Node R (Node B t2 x2 t3) x3 t4) = (Node B (balanceR B t1 x1 (Node R t2 x2 t3)) x3 t4, False)
unbalancedR _ _ _ _ = error "unbalancedR"

----------------------------------------------------------------

deleteMin :: RBTree a -> RBTree a
deleteMin t = s
  where
    ((s, _), _) = deleteMin' t

deleteMin' :: RBTree a -> (RBTreeBDel a, a)
deleteMin' Leaf                           = error "deleteMin'"
deleteMin' (Node B Leaf x Leaf)           = ((Leaf, True), x)
deleteMin' (Node B Leaf x (Node R l y r)) = ((Node B l y r, False), x)
deleteMin' (Node R Leaf x r)              = ((r, False), x)
deleteMin' (Node c l x r)                 = if d then (tD, m) else (tD', m)
  where
    ((l',d),m) = deleteMin' l
    tD  = unbalancedR c l' x r
    tD' = (Node c l' x r, False)

----------------------------------------------------------------

blackify :: RBTree a -> RBTreeBDel a
blackify s@(Node R _ _ _) = (turnB s, False)
blackify s                = (s, True)

delete :: Ord a => a -> RBTree a -> RBTree a
delete x t = s
  where
    (s,_) = delete' x t

delete' :: Ord a => a -> RBTree a -> RBTreeBDel a
delete' _ Leaf = (Leaf, False)
delete' x (Node c l y r) = case compare x y of
    LT -> let (l',d) = delete' x l
              t = Node c l' y r
          in if d then unbalancedR c l' y r else (t, False)
    GT -> let (r',d) = delete' x r
              t = Node c l y r'
          in if d then unbalancedL c l y r' else (t, False)
    EQ -> case r of
        Leaf -> if c == B then blackify l else (l, False)
        _ -> let ((r',d),m) = deleteMin' r
                 t = Node c l m r'
             in if d then unbalancedL c l m r' else (t, False)
-}