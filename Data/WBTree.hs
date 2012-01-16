{-| Purely functional weight balanced trees, aka trees of bounded balanced.

-}

module Data.WBTree (
  -- * Data structures
    WBTree(..)
  , Size
  , size
  -- * Creating red-black trees
  , empty
  , singleton
  , insert
  , fromList
  -- * Converting a list
  , toList
  -- * Membership
  , member
  -- * Deleting
  , delete
  , deleteMin
  , deleteMax
  -- * Checking
  , null
  -- * Set operations
  , union
  , intersection
  , difference
  -- * Helper functions
  , join
  , merge
  , split
  , valid
  , minimum
  , maximum
--  , showTree
--  , printTree
  ) where

import Data.List (foldl')
import Prelude hiding (minimum, maximum, null)

----------------------------------------------------------------

type Size = Int
data WBTree a = Leaf | Node Size a (WBTree a) (WBTree a) deriving (Show)


instance (Eq a) => Eq (WBTree a) where
    t1 == t2 = toList t1 == toList t2

size :: WBTree a -> Size
size Leaf            = 0
size (Node sz _ _ _) = sz

----------------------------------------------------------------

{-|
See if the red black tree is empty.

>>> Data.WBTree.null empty
True
>>> Data.WBTree.null (singleton 1)
False
-}

null :: Eq a => WBTree a -> Bool
null t = t == Leaf

----------------------------------------------------------------

{-| Empty tree.

>>> size empty
0
-}

empty :: WBTree a
empty = Leaf

{-| Singleton tree.

>>> size (singleton 'a')
1
-}

singleton :: a -> WBTree a
singleton k = Node 1 k Leaf Leaf

----------------------------------------------------------------

node :: a -> WBTree a -> WBTree a -> WBTree a
node k l r = Node (size l + size r + 1) k l r

----------------------------------------------------------------

{-| Insertion. O(log N)

>>> insert 5 (fromList [5,3]) == fromList [3,5]
True
>>> insert 7 (fromList [5,3]) == fromList [3,5,7]
True
>>> insert 5 empty            == singleton 5
True
-}

insert :: Ord a => a -> WBTree a -> WBTree a
insert kx Leaf = singleton kx
insert kx (Node sz ky l r) = case compare kx ky of
    LT -> balanceR ky (insert kx l) r
    GT -> balanceL ky l (insert kx r)
    EQ -> Node sz kx l r

{-| Creating a tree from a list. O(N log N)

>>> empty == fromList []
True
>>> singleton 'a' == fromList ['a']
True
>>> fromList [5,3,5] == fromList [5,3]
True
-}

fromList :: Ord a => [a] -> WBTree a
fromList = foldl' (flip insert) empty

----------------------------------------------------------------

{-| Creating a list from a tree. O(N)

>>> toList (fromList [5,3])
[3,5]
>>> toList empty
[]
-}

toList :: WBTree a -> [a]
toList t = inorder t []
  where
    inorder Leaf xs = xs
    inorder (Node _ x l r) xs = inorder l (x : inorder r xs)

----------------------------------------------------------------

{-| Checking if this element is a member of a tree?

>>> member 5 (fromList [5,3])
True
>>> member 1 (fromList [5,3])
False
-}

member :: Ord a => a -> WBTree a -> Bool
member _ Leaf = False
member x (Node _ y l r) = case compare x y of
    LT -> member x l
    GT -> member x r
    EQ -> True

----------------------------------------------------------------

balanceL :: a -> WBTree a -> WBTree a -> WBTree a
balanceL k l r
  | isBalanced l r = node k l r
  | otherwise      = rotateL k l r

balanceR :: a -> WBTree a -> WBTree a -> WBTree a
balanceR k l r
  | isBalanced r l = node k l r
  | otherwise      = rotateR k l r

rotateL :: a -> WBTree a -> WBTree a -> WBTree a
rotateL k l r@(Node _ _ rl rr)
  | isSingle rl rr = singleL k l r
  | otherwise      = doubleL k l r
rotateL _ _ _      = error "rotateL"

rotateR :: a -> WBTree a -> WBTree a -> WBTree a
rotateR k l@(Node _ _ ll lr) r
  | isSingle lr ll = singleR k l r
  | otherwise      = doubleR k l r
rotateR _ _ _      = error "rotateR"

singleL :: a -> WBTree a -> WBTree a -> WBTree a
singleL k1 t1 (Node _ k2 t2 t3) = node k2 (node k1 t1 t2) t3
singleL _ _ _                   = error "singleL"

singleR :: a -> WBTree a -> WBTree a -> WBTree a
singleR k1 (Node _ k2 t1 t2) t3 = node k2 t1 (node k1 t2 t3)
singleR _ _ _                   = error "singleR"


doubleL :: a -> WBTree a -> WBTree a -> WBTree a
doubleL k1 t1 (Node _ k2 (Node _ k3 t2 t3) t4) = node k3 (node k1 t1 t2) (node k2 t3 t4)
doubleL _ _ _ = error "doubleL"

doubleR :: a -> WBTree a -> WBTree a -> WBTree a
doubleR k1 (Node _ k2 t1 (Node _ k3 t2 t3)) t4 = node k3 (node k2 t1 t2) (node k1 t3 t4)
doubleR _ _ _ = error "doubleR"

----------------------------------------------------------------

{-| Deleting the minimum element. O(log N)

>>> deleteMin (fromList [5,3,7]) == fromList [5,7]
True
>>> deleteMin empty == empty
True
-}

deleteMin :: WBTree a -> WBTree a
deleteMin (Node _ _  Leaf r) = r
deleteMin (Node _ kx l r)    = balanceL kx (deleteMin l) r
deleteMin Leaf               = Leaf

{-| Deleting the maximum

>>> deleteMax (fromList [(5,"a"), (3,"b"), (7,"c")]) == fromList [(3,"b"), (5,"a")]
True
>>> deleteMax empty == empty
True
-}

deleteMax :: WBTree a -> WBTree a
deleteMax (Node _ _ l Leaf)  = l
deleteMax (Node _ kx l r)    = balanceR kx l (deleteMax r)
deleteMax Leaf               = Leaf

----------------------------------------------------------------

{-| Deleting this element from a tree. O(log N)

>>> delete 5 (fromList [5,3]) == singleton 3
True
>>> delete 7 (fromList [5,3]) == fromList [3,5]
True
>>> delete 5 empty            == empty
True
-}

delete :: Ord a => a -> WBTree a -> WBTree a
delete k t
  = case t of
      Leaf -> Leaf
      Node _ kx l r
          -> case compare k kx of
               LT -> balanceL kx (delete k l) r
               GT -> balanceR kx l (delete k r)
               EQ -> glue l r

----------------------------------------------------------------

valid :: Ord a => WBTree a -> Bool
valid t
  = balanced t && ordered t && validsize t

balanced :: WBTree a -> Bool
balanced Leaf           = True
balanced (Node _ _ l r) = isBalanced l r && isBalanced r l
                       && balanced l     && balanced r

ordered :: Ord a => WBTree a -> Bool
ordered t
  = bounded (const True) (const True) t
  where
    bounded lo hi t'
      = case t' of
          Leaf              -> True
          Node _ kx l r  -> lo kx && hi kx && bounded lo (<kx) l && bounded (>kx) hi r

validsize :: WBTree a -> Bool
validsize t
  = (realsize t == Just (size t))
  where
    realsize t'
      = case t' of
          Leaf            -> Just 0
          Node sz _ l r -> case (realsize l,realsize r) of
              (Just n,Just m)  | n+m+1 == sz  -> Just sz
              _                               -> Nothing

----------------------------------------------------------------

join :: Ord a => WBTree a -> a -> WBTree a -> WBTree a
join Leaf kx r   = insert kx r
join l kx Leaf   = insert kx l
join l@(Node _ ky ly ry) kx r@(Node _ kz lz rz)
  | bal1 && bal2 = node kx l r
  | bal1         = balanceL ky ly (join ry kx r)
  | otherwise    = balanceR kz (join l kx lz) rz
  where
    bal1 = isBalanced l r
    bal2 = isBalanced r l

merge :: WBTree a -> WBTree a -> WBTree a
merge Leaf r   = r
merge l Leaf   = l
merge l@(Node _ kx lx rx) r@(Node _ ky ly ry)
  | bal1 && bal2 = glue l r
  | bal1         = balanceL kx lx (merge rx r)
  | otherwise    = balanceR ky (merge l ly) ry
  where
    bal1 = isBalanced l r
    bal2 = isBalanced r l

glue :: WBTree a -> WBTree a -> WBTree a
glue Leaf r = r
glue l Leaf = l
glue l r
  | size l > size r = let km = maximum l
                          l' = deleteMax l
                      in balanceL km l' r
  | otherwise       = let km = minimum r
                          r' = deleteMin r
                      in balanceR km l r'

{-| Splitting a tree. O(log N)

>>> split 2 (fromList [5,3]) == (empty, fromList [3,5])
True
>>> split 3 (fromList [5,3]) == (empty, singleton 5)
True
>>> split 4 (fromList [5,3]) == (singleton 3, singleton 5)
True
>>> split 5 (fromList [5,3]) == (singleton 3, empty)
True
>>> split 6 (fromList [5,3]) == (fromList [3,5], empty)
True
-}

split :: Ord a => a -> WBTree a -> (WBTree a, WBTree a)
split _ Leaf = (Leaf,Leaf)
split k (Node _ kx l r)
  = case compare k kx of
      LT -> let (lt,gt) = split k l in (lt,join gt kx r)
      GT -> let (lt,gt) = split k r in (join l kx lt,gt)
      EQ -> (l,r)

----------------------------------------------------------------

{-| Finding the minimum element. O(log N)

>>> minimum (fromList [3,5,1])
1
>>> minimum empty
*** Exception: minimum
-}

minimum :: WBTree a -> a
minimum (Node _ x Leaf _) = x
minimum (Node _ _ l _)    = minimum l
minimum _                 = error "minimum"

{-| Finding the maximum element. O(log N)

>>> maximum (fromList [3,5,1])
5
>>> maximum empty
*** Exception: maximum
-}

maximum :: WBTree a -> a
maximum (Node _ x _ Leaf) = x
maximum (Node _ _ _ r)    = maximum r
maximum _                 = error "maximum"

----------------------------------------------------------------

{-| Creating a union tree from two trees. O(N + M)

>>> union (fromList [5,3]) (fromList [5,7]) == fromList [3,5,7]
True
-}

union :: Ord a => WBTree a -> WBTree a -> WBTree a
union t1 Leaf = t1
union Leaf t2 = t2
union t1 (Node _ x l r) = join (union l' l) x (union r' r)
  where
    (l',r') = split x t1

{-| Creating a intersection tree from trees. O(N + N)

>>> intersection (fromList [5,3]) (fromList [5,7]) == singleton 5
True
-}

intersection :: Ord a => WBTree a -> WBTree a -> WBTree a
intersection Leaf _ = Leaf
intersection _ Leaf = Leaf
intersection t1 (Node _ x l r)
  | member x t1 = join (intersection l' l) x (intersection r' r)
  | otherwise   = merge (intersection l' l) (intersection r' r)
  where
    (l',r') = split x t1

{-| Creating a difference tree from trees. O(N + N)

>>> difference (fromList [5,3]) (fromList [5,7]) == singleton 3
True
-}

difference :: Ord a => WBTree a -> WBTree a -> WBTree a
difference Leaf _  = Leaf
difference t1 Leaf = t1
difference t1 (Node _ x l r) = merge (difference l' l) (difference r' r)
  where
    (l',r') = split x t1

----------------------------------------------------------------

delta :: Int
delta = 3

gamma :: Int
gamma = 2

isBalanced :: WBTree a -> WBTree a -> Bool
isBalanced a b = delta * (size a + 1) >= (size b + 1)

isSingle :: WBTree a -> WBTree a -> Bool
isSingle a b = (size a + 1) < gamma * (size b + 1)
