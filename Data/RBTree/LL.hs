module Data.RBTree.LL (
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
  , minimum
  , maximum
  , join
  , valid
  , showTree
  ) where

import Data.List (foldl')
import Data.RBTree.Internal
import Prelude hiding (minimum, maximum)

----------------------------------------------------------------

valid :: RBTree a -> Bool
valid t = isBalanced t && isLeftLean t && blackHeight t

isLeftLean :: RBTree a -> Bool
isLeftLean Leaf = True
isLeftLean (Node B _ _ _ (Node R _ _ _ _)) = False -- right only and both!
isLeftLean (Node _ _ r _ l) = isLeftLean r && isLeftLean l

----------------------------------------------------------------

fromList :: Ord a => [a] -> RBTree a
fromList = foldl' (flip insert) empty

----------------------------------------------------------------

insert :: Ord a => a -> RBTree a -> RBTree a
insert kx t = turnB (insert' kx t)

insert' :: Ord a => a -> RBTree a -> RBTree a
insert' kx Leaf = Node R 1 Leaf kx Leaf
insert' kx t@(Node c h l x r) = case compare kx x of
    LT -> balanceL c h (insert' kx l) x r
    GT -> balanceR c h l x (insert' kx r)
    EQ -> t

balanceL :: Color -> BlackHeight -> RBTree a -> a -> RBTree a -> RBTree a
balanceL B h (Node R _ ll@(Node R _ _ _ _) lx lr) x r =
    Node R (h+1) (turnB ll) lx (Node B h lr x r)
balanceL c h l x r = Node c h l x r

balanceR :: Color -> BlackHeight -> RBTree a -> a -> RBTree a -> RBTree a
balanceR B h l@(Node R _ _ _ _) x r@(Node R _ _ _ _) =
    Node R (h+1) (turnB l) x (turnB r)
-- x is Black since Red eliminated by the case above
-- x is either Node or Leaf
balanceR c h l x (Node R rh rl rx rr) = Node c h (Node R rh l x rl) rx rr
balanceR c h l x r = Node c h l x r

----------------------------------------------------------------

isBlackLeftBlack :: RBTree a -> Bool
isBlackLeftBlack (Node B _ Leaf _ _)             = True
isBlackLeftBlack (Node B _ (Node B _ _ _ _) _ _) = True
isBlackLeftBlack _                               = False


isBlackLeftRed :: RBTree a -> Bool
isBlackLeftRed (Node B _ (Node R _ _ _ _) _ _) = True
isBlackLeftRed _                               = False

----------------------------------------------------------------

deleteMin :: RBTree a -> RBTree a
deleteMin t = case deleteMin' (turnR t) of
    Leaf -> Leaf
    s    -> turnB s

{-
  This deleteMin' keeps an invariant: the target node is always red.

  If the left child of the minimum node is Leaf, the right child
  MUST be Leaf thanks to the invariants of LLRB.
-}

deleteMin' :: RBTree a -> RBTree a
deleteMin' (Node R _ Leaf _ Leaf) = Leaf -- deleting the minimum
deleteMin' t@(Node R h l x r)
  -- Red
  | isRed l      = Node R h (deleteMin' l) x r
  -- Black-Black
  | isBB && isBR = hardMin t
  | isBB         = balanceR B (h-1) (deleteMin' (turnR l)) x (turnR r)
  -- Black-Red
  | otherwise    = Node R h (Node B lh (deleteMin' ll) lx lr) x r -- ll is Red
  where
    isBB = isBlackLeftBlack l
    isBR = isBlackLeftRed r
    Node B lh ll lx lr = l -- to skip Black
deleteMin' _ = error "deleteMin'"

-- Simplified but not keeping the invariant.
{-
deleteMin' :: RBTree a -> RBTree a
deleteMin' (Node R _ Leaf _ Leaf) = Leaf
deleteMin' t@(Node R h l x r)
  | isBB && isBR = hardMin t
  | isBB         = balanceR B (h-1) (deleteMin' (turnR l)) x (turnR r)
  where
    isBB = isBlackLeftBlack l
    isBR = isBlackLeftRed r
deleteMin' (Node c h l x r) = Node c h (deleteMin' l) x r
deleteMin' _ = error "deleteMin'"
-}

{-
  The hardest case. See slide 61 of:
	http://www.cs.princeton.edu/~rs/talks/LLRB/RedBlack.pdf
-}

hardMin :: RBTree a -> RBTree a
hardMin (Node R h l x (Node B rh (Node R _ rll rlx rlr) rx rr))
    = Node R h (Node B rh (deleteMin' (turnR l)) x rll)
               rlx
               (Node B rh rlr rx rr)
hardMin _ = error "hardMin"

----------------------------------------------------------------

deleteMax :: RBTree a -> RBTree a
deleteMax t = case deleteMax' (turnR t) of
    Leaf -> Leaf
    s    -> turnB s

{-
  This deleteMax' keeps an invariant: the target node is always red.

  If the right child of the minimum node is Leaf, the left child
  is:

  1) A Leaf -- we can delete it
  2) A red node -- we can rotateR it and have 1).
-}

deleteMax' :: RBTree a -> RBTree a
deleteMax' (Node R _ Leaf _ Leaf) = Leaf -- deleting the maximum
deleteMax' t@(Node R h l x r)
  | isRed l      = rotateR t
  -- Black-Black
  | isBB && isBR = hardMax t
  | isBB         = balanceR B (h-1) (turnR l) x (deleteMax' (turnR r))
  -- Black-Red
  | otherwise    = Node R h l x (rotateR r)
  where
    isBB = isBlackLeftBlack r
    isBR = isBlackLeftRed l
deleteMax' _ = error "deleteMax'"

-- Simplified but not keeping the invariant.
{-
deleteMax' :: RBTree a -> RBTree a
deleteMax' (Node R _ Leaf _ Leaf) = Leaf
deleteMax' t@(Node _ _ (Node R _ _ _ _) _ _) = rotateR t
deleteMax' t@(Node R h l x r)
  | isBB && isBR = hardMax t
  | isBB         = balanceR B (h-1) (turnR l) x (deleteMax' (turnR r))
  where
    isBB = isBlackLeftBlack r
    isBR = isBlackLeftRed l
deleteMax' (Node R h l x r) = Node R h l x (deleteMax' r)
deleteMax' _ = error "deleteMax'"
-}

{-
  rotateR ensures that the maximum node is in the form of (Node R Leaf _ Leaf).
-}

rotateR :: RBTree a -> RBTree a
rotateR (Node c h (Node R _ ll lx lr) x r) = balanceR c h ll lx (deleteMax' (Node R h lr x r))
rotateR _ = error "rorateR"

{-
  The hardest case. See slide 56 of:
	http://www.cs.princeton.edu/~rs/talks/LLRB/RedBlack.pdf
-}

hardMax :: RBTree a -> RBTree a
hardMax (Node R h (Node B lh ll@(Node R _ _ _ _ ) lx lr) x r)
    = Node R h (turnB ll) lx (balanceR B lh lr x (deleteMax' (turnR r)))
hardMax _              = error "hardMax"

----------------------------------------------------------------

delete :: Ord a => a -> RBTree a -> RBTree a
delete kx t = case delete' kx (turnR t) of
    Leaf -> Leaf
    t'   -> turnB t'

delete' :: Ord a => a -> RBTree a -> RBTree a
delete' _ Leaf = Leaf
delete' kx (Node c h l x r) = case compare kx x of
    LT -> deleteLT kx c h l x r
    GT -> deleteGT kx c h l x r
    EQ -> deleteEQ kx c h l x r

deleteLT :: Ord a => a -> Color -> BlackHeight -> RBTree a -> a -> RBTree a -> RBTree a
deleteLT kx R h l x r
  | isBB && isBR = Node R h (Node B rh (delete' kx (turnR l)) x rll) rlx (Node B rh rlr rx rr)
  | isBB         = balanceR B (h-1) (delete' kx (turnR l)) x (turnR r)
  where
    isBB = isBlackLeftBlack l
    isBR = isBlackLeftRed r
    Node B rh (Node R _ rll rlx rlr) rx rr = r
deleteLT kx c h l x r = Node c h (delete' kx l) x r

deleteGT :: Ord a => a -> Color -> BlackHeight -> RBTree a -> a -> RBTree a -> RBTree a
deleteGT kx c h (Node R _ ll lx lr) x r = balanceR c h ll lx (delete' kx (Node R h lr x r))
deleteGT kx R h l x r
  | isBB && isBR = Node R h (turnB ll) lx (balanceR B lh lr x (delete' kx (turnR r)))
  | isBB         = balanceR B (h-1) (turnR l) x (delete' kx (turnR r))
  where
    isBB = isBlackLeftBlack r
    isBR = isBlackLeftRed l
    Node B lh ll@(Node R _ _ _ _) lx lr = l
deleteGT kx R h l x r = Node R h l x (delete' kx r)
deleteGT _ _ _ _ _ _ = error "deleteGT"

deleteEQ :: Ord a => a -> Color -> BlackHeight -> RBTree a -> a -> RBTree a -> RBTree a
deleteEQ _ R _ Leaf _ Leaf = Leaf
deleteEQ kx c h (Node R _ ll lx lr) x r = balanceR c h ll lx (delete' kx (Node R h lr x r))
deleteEQ _ R h l _ r
  | isBB && isBR = balanceR R h (turnB ll) lx (balanceR B lh lr m (deleteMin' (turnR r)))
  | isBB         = balanceR B (h-1) (turnR l) m (deleteMin' (turnR r))
  where
    isBB = isBlackLeftBlack r
    isBR = isBlackLeftRed l
    Node B lh ll@(Node R _ _ _ _) lx lr = l
    m = minimum r
deleteEQ _ R h l _ r@(Node B rh rl rx rr) = Node R h l m (Node B rh (deleteMin' rl) rx rr) -- rl is Red
  where
    m = minimum r
deleteEQ _ _ _ _ _ _ = error "deleteEQ"

----------------------------------------------------------------

minimum :: RBTree a -> a
minimum (Node _ _ Leaf x _) = x
minimum (Node _ _ l _ _)    = minimum l
minimum _                   = error "minimum"

maximum :: RBTree a -> a
maximum (Node _ _ _ x Leaf) = x
maximum (Node _ _ _ _ r)    = maximum r
maximum _                   = error "maximum"

----------------------------------------------------------------

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

joinLT :: Ord a => RBTree a -> a -> RBTree a -> BlackHeight -> RBTree a
joinLT t1 g t2@(Node c h l x r) h1
  | h == h1   = Node R (h+1) t1 g t2
  | otherwise = balanceL c h (joinLT t1 g l h1) x r
joinLT _ _ _ _ = error "joinLT"

joinGT :: Ord a => RBTree a -> a -> RBTree a -> BlackHeight -> RBTree a
joinGT t1@(Node c h l x r) g t2 h2
  | h == h2   = Node R (h+1) t1 g t2
  | otherwise = balanceR c h l x (joinGT r g t2 h2)
joinGT _ _ _ _ = error "joinGT"
