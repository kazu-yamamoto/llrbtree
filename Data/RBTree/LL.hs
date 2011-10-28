module Data.RBTree.LL (
    RBTree(..)
  , Color(..)
  , empty
  , insert
  , fromList
  , toList
  , member
--  , delete
  , deleteMin
  , deleteMax
  , valid
  ) where

import Data.List (foldl')
import Data.RBTree.Internal
import Prelude hiding (minimum)

----------------------------------------------------------------

valid :: RBTree a -> Bool
valid t = isBalanced t && isLeftLean t

isLeftLean :: RBTree a -> Bool
isLeftLean Leaf = True
isLeftLean (Fork B _ _ (Fork R _ _ _)) = False -- right only and both!
isLeftLean (Fork _ r _ l) = isLeftLean r && isLeftLean l

----------------------------------------------------------------

fromList :: Ord a => [a] -> RBTree a
fromList = foldl' (flip insert) empty

----------------------------------------------------------------

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
balanceL k a x b = Fork k a x b

balanceR :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balanceR B (Fork R a x b) y (Fork R c z d) =
    Fork R (Fork B a x b) y (Fork B c z d)
-- x is Black since Red eliminated by the case above
-- x is either Fork or Leaf
balanceR k x y (Fork R c z d) =
    Fork k (Fork R x y c) z d
balanceR k a x b = Fork k a x b

----------------------------------------------------------------

deleteMin :: RBTree a -> RBTree a
deleteMin t = case deleteMin' (turnR t) of
    Leaf -> Leaf
    t'   -> turnB t'

{-
  This deleteMin' keeps an invariant: the target node is always red.

  If the left child of the minimum node is Leaf, the right child
  MUST be Leaf thanks to the invariants of LLRB.
-}

deleteMin' :: RBTree a -> RBTree a
deleteMin' (Fork R Leaf _ Leaf) = Leaf -- deleting the minimum
deleteMin' t@(Fork R l x r)
  -- Red
  | isRed l      = Fork R (deleteMin' l) x r
  -- Black-Black
  | isBB && isBR = hardMin t
  | isBB         = balanceR B (deleteMin' (turnR l)) x (turnR r)
  -- Black-Red
  | otherwise    = Fork R (Fork B (deleteMin' la) lx lb) x r -- la is Red
  where
    isBB = isBlack (left l)
    isBR = isRed (left r)
    Fork B la lx lb = l -- to skip Black
deleteMin' _ = error "deleteMin'"

-- Simplified but not keeping the invariant.

{-
deleteMin' :: RBTree a -> RBTree a
deleteMin' (Fork R Leaf _ Leaf) = Leaf
deleteMin' t@(Fork R l x r)
  | isBB && isBR = hardMin t
  | isBB         = balanceR B (deleteMin' (turnR l)) x (turnR r)
  where
    isBB = isBlack l && isBlack (left l)
    isBR = isRed (left r)
deleteMin' (Fork k l x r) = Fork k (deleteMin' l) x r
deleteMin' _ = error "deleteMin'"
-}

{-
  The hardest case. See slide 61 of:
	http://www.cs.princeton.edu/~rs/talks/LLRB/RedBlack.pdf
-}

hardMin :: RBTree a -> RBTree a
hardMin (Fork R l x (Fork B (Fork R b y c) z d))
    = Fork R (Fork B (deleteMin' (turnR l)) x b) y (Fork B c z d)
hardMin _ = error "hardMin"

----------------------------------------------------------------

deleteMax :: RBTree a -> RBTree a
deleteMax t = case deleteMax' (turnR t) of
    Leaf -> Leaf
    t'   -> turnB t'

{-
  This deleteMax' keeps an invariant: the target node is always red.

  If the right child of the minimum node is Leaf, the left child
  is:

  1) A Leaf -- we can delete it
  2) A red node -- we can rotateR it and have 1).
-}

deleteMax' :: RBTree a -> RBTree a
deleteMax' (Fork R Leaf _ Leaf) = Leaf -- deleting the maximum
deleteMax' t@(Fork R l x r)
  | isRed l      = rotateR t
  -- Black-Black
  | isBB && isBR = hardMax t
  | isBB         = balanceR B (turnR l) x (deleteMax' (turnR r))
  -- Black-Red
  | otherwise    = Fork R l x (rotateR r)
  where
    isBB = isBlack (left r)
    isBR = isRed (left l) -- && isBlack l
deleteMax' _ = error "deleteMax'"

-- Simplified but not keeping the invariant.
{-
deleteMax' :: RBTree a -> RBTree a
deleteMax' (Fork R Leaf _ Leaf) = Leaf
deleteMax' t@(Fork _ (Fork R _ _ _) _ _) = rotateR t
deleteMax' t@(Fork R l x r)
  | isBB && isBR = hardMax t
  | isBB         = balanceR B (turnR l) x (deleteMax' (turnR r))
  where
    isBB = isBlack (left r)
    isBR = isRed (left l) -- && isBlack l
deleteMax' (Fork k l x r) = Fork k l x (deleteMax' r)
deleteMax' _ = error "deleteMax'"
-}

{-
  rotateR ensures that the maximum node is in the form of (Fork R Leaf _ Leaf).
-}

rotateR :: RBTree a -> RBTree a
rotateR (Fork k (Fork R a x b) y c) = balanceR k a x (deleteMax' (Fork R b y c))
rotateR _ = error "rorateR"

{-
  The hardest case. See slide 56 of:
	http://www.cs.princeton.edu/~rs/talks/LLRB/RedBlack.pdf
-}

hardMax :: RBTree a -> RBTree a
hardMax (Fork R (Fork B la@(Fork R _ _ _) lx lb) x r)
    = Fork R (turnB la) lx (balanceR B lb x (deleteMax' (turnR r)))
hardMax _              = error "hardMax"

----------------------------------------------------------------

{-
delete :: Ord a => a -> RBTree a -> RBTree a
delete kx t = case delete' kx (turnR t) of
    Leaf -> Leaf
    t'   -> turnB t'

delete' :: Ord a => a -> RBTree a -> RBTree a
delete' _ Leaf = Leaf
delete' kx (Fork k l x r) = case compare kx x of
    LT -> deleteLT kx k l x r
    GT -> deleteGT kx k l x r
    EQ -> deleteEQ kx k l x r

deleteLT :: Ord a => a -> Color -> RBTree a -> a -> RBTree a -> RBTree a
deleteLT kx R l x r
  | isBB && isBR = Fork R (Fork B (delete' kx (turnR l)) x b) y (Fork B c z d)
  | isBB         = balanceR B (delete' kx (turnR l)) x (turnR r)
  where
    isBB = isBlackLeftBlack l
    isBR = isBlackLeftRed r
    Fork B (Fork R b y c) z d = r
deleteLT kx k l x r = Fork k (delete' kx l) x r

deleteGT :: Ord a => a -> Color -> RBTree a -> a -> RBTree a -> RBTree a
deleteGT kx k (Fork R a x b) y c = balanceR k a x (delete' kx (Fork R b y c))
deleteGT kx R l y r
  | isBB && isBR = Fork R (turnB a) x (balanceR B b y (delete' kx (turnR r)))
  | isBB         = balanceR B (turnR l) y (delete' kx (turnR r))
  where
    isBB = isBlackLeftBlack r
    isBR = isBlackLeftRed l
    Fork B a@(Fork R _ _ _) x b = l
deleteGT kx k l x r = Fork k l x (delete' kx r)

deleteEQ :: Ord a => a -> Color -> RBTree a -> a -> RBTree a -> RBTree a
deleteEQ _ _ Leaf _ Leaf = Leaf
deleteEQ kx k (Fork R a x b) y c = balanceR k a x (delete' kx (Fork R b y c))
deleteEQ kx R l y r
  | isBB && isBR = balanceR R (turnB a) x (delete' kx (Fork B b y (turnR r)))
  | isBB         = balanceR B (turnR l) m (deleteMin r) -- xxx (turnR r)
  where
    isBB = isBlackLeftBlack r
    isBR = isBlackLeftRed l
    Fork B a@(Fork R _ _ _) x b = l
    m = minimum r
deleteEQ _ k l _ r = Fork k l m (deleteMin r)
  where
    m = minimum r

minimum :: RBTree a -> a
minimum (Fork _ Leaf x _) = x
minimum (Fork _ l _ _) = minimum l
minimum _ = error "minimum"
-}