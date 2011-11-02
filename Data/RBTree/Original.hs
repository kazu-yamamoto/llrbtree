module Data.RBTree.Original (
    insert
  , delete
  , deleteMin
  , deleteMax
  , balanceL
  , balanceR
  , valid
  ) where

import Data.RBTree.Internal

----------------------------------------------------------------

valid :: RBTree a -> Bool
valid t = isBalanced t && blackHeight t

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

----------------------------------------------------------------

type RBTreeBDel a = (RBTree a, Bool)

unbalancedL :: Color -> BlackHeight -> RBTree a -> a -> RBTree a -> RBTreeBDel a
unbalancedL c h l@(Node B _ _ _ _) x r
  = (balanceL B h (turnR l) x r, c == B)
unbalancedL B h (Node R lh ll lx lr@(Node B _ _ _ _)) x r
  = (Node B lh ll lx (balanceL B h (turnR lr) x r), False)
unbalancedL _ _ _ _ _ = error "unbalancedL"

-- The left tree lacks one Black node
unbalancedR :: Color -> BlackHeight -> RBTree a -> a -> RBTree a -> (RBTree a, Bool)
-- Decreasing one Black node in the right
unbalancedR c h l x r@(Node B _ _ _ _)
  = (balanceR B h l x (turnR r), c == B)
-- Taking one Red node from the right and adding it to the right as Black
unbalancedR B h l x (Node R rh rl@(Node B _ _ _ _) rx rr)
  = (Node B rh (balanceR B h l x (turnR rl)) rx rr, False)
unbalancedR _ _ _ _ _ = error "unbalancedR"

----------------------------------------------------------------

deleteMin :: RBTree a -> RBTree a
deleteMin t = turnB' s
  where
    ((s, _), _) = deleteMin' t

deleteMin' :: RBTree a -> (RBTreeBDel a, a)
deleteMin' Leaf                           = error "deleteMin'"
deleteMin' (Node B _ Leaf x Leaf)         = ((Leaf, True), x)
deleteMin' (Node B _ Leaf x r@(Node R _ _ _ _)) = ((turnB r, False), x)
deleteMin' (Node R _ Leaf x r)            = ((r, False), x)
deleteMin' (Node c h l x r)               = if d then (tD, m) else (tD', m)
  where
    ((l',d),m) = deleteMin' l
    tD  = unbalancedR c (h-1) l' x r
    tD' = (Node c h l' x r, False)

----------------------------------------------------------------

deleteMax :: RBTree a -> RBTree a
deleteMax t = turnB' s
  where
    ((s, _), _) = deleteMax' t

deleteMax' :: RBTree a -> (RBTreeBDel a, a)
deleteMax' Leaf                           = error "deleteMax'"
deleteMax' (Node B _ Leaf x Leaf)         = ((Leaf, True), x)
deleteMax' (Node B _ l@(Node R _ _ _ _) x Leaf) = ((turnB l, False), x)
deleteMax' (Node R _ l x Leaf)            = ((l, False), x)
deleteMax' (Node c h l x r)               = if d then (tD, m) else (tD', m)
  where
    ((r',d),m) = deleteMax' r
    tD  = unbalancedL c (h-1) l x r'
    tD' = (Node c h l x r', False)

----------------------------------------------------------------

blackify :: RBTree a -> RBTreeBDel a
blackify s@(Node R _ _ _ _) = (turnB s, False)
blackify s                  = (s, True)

delete :: Ord a => a -> RBTree a -> RBTree a
delete x t = turnB' s
  where
    (s,_) = delete' x t

delete' :: Ord a => a -> RBTree a -> RBTreeBDel a
delete' _ Leaf = (Leaf, False)
delete' x (Node c h l y r) = case compare x y of
    LT -> let (l',d) = delete' x l
              t = Node c h l' y r
          in if d then unbalancedR c (h-1) l' y r else (t, False)
    GT -> let (r',d) = delete' x r
              t = Node c h l y r'
          in if d then unbalancedL c (h-1) l y r' else (t, False)
    EQ -> case r of
        Leaf -> if c == B then blackify l else (l, False)
        _ -> let ((r',d),m) = deleteMin' r
                 t = Node c h l m r'
             in if d then unbalancedL c (h-1) l m r' else (t, False)
