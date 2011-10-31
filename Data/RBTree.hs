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
insert kx t = turnB (ins t)
  where
    ins Leaf = Node R Leaf kx Leaf
    ins s@(Node k l x r) = case compare kx x of
        LT -> balanceL k (ins l) x r
        GT -> balanceR k l x (ins r)
        EQ -> s

balanceL :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balanceL B (Node R (Node R a x b) y c) z d =
    Node R (Node B a x b) y (Node B c z d)
balanceL B (Node R a x (Node R b y c)) z d =
    Node R (Node B a x b) y (Node B c z d)
balanceL k l x r = Node k l x r

balanceR :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balanceR B a x (Node R b y (Node R c z d)) =
    Node R (Node B a x b) y (Node B c z d)
balanceR B a x (Node R (Node R b y c) z d) =
    Node R (Node B a x b) y (Node B c z d)
balanceR k l x r = Node k l x r

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
-- Takeing one Red node from the right and adding it to the right as Black
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
