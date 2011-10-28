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
insert x t = turnB (insert' x t)

insert' :: Ord a => a -> RBTree a -> RBTree a
insert' kx Leaf = Fork R Leaf kx Leaf
insert' kx t@(Fork c l x r) = case compare kx x of
    LT -> balanceL c (insert' kx l) x r
    GT -> balanceR c l x (insert' kx r)
    EQ -> t

balanceL :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balanceL B (Fork R ll@(Fork R _ _ _) lx lr) x r =
    Fork R (turnB ll) lx (Fork B lr x r)
balanceL c l x r = Fork c l x r

balanceR :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balanceR B l@(Fork R _ _ _) x r@(Fork R _ _ _) = Fork R (turnB l) x (turnB r)
-- x is Black since Red eliminated by the case above
-- x is either Fork or Leaf
balanceR c r x (Fork R rl rx rr) = Fork c (Fork R r x rl) rx rr
balanceR c l x r = Fork c l x r

----------------------------------------------------------------

isBlackLeftBlack :: RBTree a -> Bool
isBlackLeftBlack (Fork B Leaf _ _)           = True
isBlackLeftBlack (Fork B (Fork B _ _ _) _ _) = True
isBlackLeftBlack _                           = False


isBlackLeftRed :: RBTree a -> Bool
isBlackLeftRed (Fork B (Fork R _ _ _) _ _) = True
isBlackLeftRed _                           = False

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
    isBB = isBlackLeftBlack l
    isBR = isBlackLeftRed r
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
    isBB = isBlackLeftBlack l
    isBR = isBlackLeftRed r
deleteMin' (Fork c l x r) = Fork c (deleteMin' l) x r
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
    s    -> turnB s

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
    isBB = isBlackLeftBlack r
    isBR = isBlackLeftRed l
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
    isBB = isBlackLeftBlack r
    isBR = isBlackLeftRed l
deleteMax' (Fork R l x r) = Fork R l x (deleteMax' r)
deleteMax' _ = error "deleteMax'"
-}

{-
  rotateR ensures that the maximum node is in the form of (Fork R Leaf _ Leaf).
-}

rotateR :: RBTree a -> RBTree a
rotateR (Fork c (Fork R ll lx lr) x r) = balanceR c ll lx (deleteMax' (Fork R lr x r))
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

delete :: Ord a => a -> RBTree a -> RBTree a
delete kx t = case delete' kx (turnR t) of
    Leaf -> Leaf
    t'   -> turnB t'

delete' :: Ord a => a -> RBTree a -> RBTree a
delete' _ Leaf = Leaf
delete' kx (Fork c l x r) = case compare kx x of
    LT -> deleteLT kx c l x r
    GT -> deleteGT kx c l x r
    EQ -> deleteEQ kx c l x r

deleteLT :: Ord a => a -> Color -> RBTree a -> a -> RBTree a -> RBTree a
deleteLT kx R l x r
  | isBB && isBR = Fork R (Fork B (delete' kx (turnR l)) x rll) rlx (Fork B rlr rx rr)
  | isBB         = balanceR B (delete' kx (turnR l)) x (turnR r)
  where
    isBB = isBlackLeftBlack l
    isBR = isBlackLeftRed r
    Fork B (Fork R rll rlx rlr) rx rr = r
deleteLT kx c l x r = Fork c (delete' kx l) x r

deleteGT :: Ord a => a -> Color -> RBTree a -> a -> RBTree a -> RBTree a
deleteGT kx c (Fork R ll lx lr) x r = balanceR c ll lx (delete' kx (Fork R lr x r))
deleteGT kx R l x r
  | isBB && isBR = Fork R (turnB ll) lx (balanceR B lr x (delete' kx (turnR r)))
  | isBB         = balanceR B (turnR l) x (delete' kx (turnR r))
  where
    isBB = isBlackLeftBlack r
    isBR = isBlackLeftRed l
    Fork B ll@(Fork R _ _ _) lx lr = l
deleteGT kx R l x r = Fork R l x (delete' kx r)
deleteGT _ _ _ _ _ = error "deleteGT"

deleteEQ :: Ord a => a -> Color -> RBTree a -> a -> RBTree a -> RBTree a
deleteEQ _ R Leaf _ Leaf = Leaf
deleteEQ kx c (Fork R ll lx lr) x r = balanceR c ll lx (delete' kx (Fork R lr x r))
deleteEQ _ R l _ r
  | isBB && isBR = balanceR R (turnB ll) lx (balanceR B lr m (deleteMin' (turnR r)))
  | isBB         = balanceR B (turnR l) m (deleteMin' (turnR r))
  where
    isBB = isBlackLeftBlack r
    isBR = isBlackLeftRed l
    Fork B ll@(Fork R _ _ _) lx lr = l
    m = minimum r
deleteEQ _ R l _ r@(Fork B rl rx rr) = Fork R l m (Fork B (deleteMin' rl) rx rr) -- rl is Red
  where
    m = minimum r
deleteEQ _ _ _ _ _ = error "deleteEQ"

minimum :: RBTree a -> a
minimum (Fork _ Leaf x _) = x
minimum (Fork _ l _ _) = minimum l
minimum _ = error "minimum"
