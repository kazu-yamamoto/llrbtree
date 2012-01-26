{-|
  Purely functional splay sets.

   * <http://www.cs.cmu.edu/~sleator/papers/self-adjusting.pdf>
-}

module Data.Set.BUSplay (
  -- * Data structures
    Splay(..)
  -- * Creating sets
  , empty
  , singleton
  , insert
  , fromList
  -- * Converting a list
  , toList
  -- * Membership
  , member
  -- * Deleting
--  , delete
  , deleteMin
--  , deleteMax
  -- * Checking
  , null
  -- * Set operations
  , union
--  , intersection
--  , difference
  -- * Helper functions
--  , merge
  , minimum
--  , maximum
  , valid
  , showSet
  , printSet
  ) where

import Data.List (foldl')
import Prelude hiding (minimum, maximum, null)

----------------------------------------------------------------

data Splay a = Leaf | Node (Splay a) a (Splay a) deriving Show

instance (Eq a) => Eq (Splay a) where
    t1 == t2 = toList t1 == toList t2

data LRb a = L a (Splay a) | R a (Splay a) deriving Show

type Path a = [LRb a]

search :: Ord a => a -> Splay a -> (Splay a, Path a)
search k s = go s []
  where
    go Leaf           bs = (Leaf, bs)
    go t@(Node l x r) bs = case compare k x of
        LT -> go l (L x r : bs)
        GT -> go r (R x l : bs)
        EQ -> (t,bs)

splay :: Splay a -> Path a -> Splay a
splay t []                 = t
splay Leaf (L x r : bs)    = splay (Node Leaf x r) bs
splay Leaf (R x l : bs)    = splay (Node l x Leaf) bs
splay (Node a x b) [L y c] = Node a x (Node b y c) -- zig
splay (Node b y c) [R x a] = Node (Node a x b) y c -- zig
splay (Node a x b) (L y c : L z d : bs)
    = splay (Node a x (Node b y (Node c z d))) bs  -- zig zig
splay (Node b x c) (R y a : L z d : bs)
    = splay (Node (Node a y b) x (Node c z d)) bs  -- zig zag
splay (Node c z d) (R y b : R x a : bs)
    = splay (Node (Node (Node a x b) y c) z d) bs  -- zig zig
splay (Node b x c) (L y d : R z a : bs)
    = splay (Node (Node a z b) x (Node c y d)) bs  -- zig zag

----------------------------------------------------------------

{-| Empty set.
-}

empty :: Splay a
empty = Leaf

{-|
See if the splay set is empty.

>>> Data.Set.BUSplay.null empty
True
>>> Data.Set.BUSplay.null (singleton 1)
False
-}

null :: Splay a -> Bool
null Leaf = True
null _ = False

{-| Singleton set.
-}

singleton :: a -> Splay a
singleton x = Node Leaf x Leaf

----------------------------------------------------------------

{-| Insertion.

>>> insert 5 (fromList [5,3]) == fromList [3,5]
True
>>> insert 7 (fromList [5,3]) == fromList [3,5,7]
True
>>> insert 5 empty            == singleton 5
True
-}

insert :: Ord a => a -> Splay a -> Splay a
insert x t = case search x t of
    (Leaf, [])         -> singleton x
    (Leaf, bs)         -> splay (singleton x) bs
    (Node l _ r, bs)   -> splay (Node l x r) bs

----------------------------------------------------------------

{-| Creating a set from a list.

>>> empty == fromList []
True
>>> singleton 'a' == fromList ['a']
True
>>> fromList [5,3,5] == fromList [5,3]
True
-}

fromList :: Ord a => [a] -> Splay a
fromList = foldl' (flip insert) empty

----------------------------------------------------------------

{-| Creating a list from a set. O(N)

>>> toList (fromList [5,3])
[3,5]
>>> toList empty
[]
-}

toList :: Splay a -> [a]
toList t = inorder t []
  where
    inorder Leaf xs = xs
    inorder (Node l x r) xs = inorder l (x : inorder r xs)

----------------------------------------------------------------

{-| Checking if this element is a member of a set?

>>> fst $ member 5 (fromList [5,3])
True
>>> fst $ member 1 (fromList [5,3])
False
-}

member :: Ord a => a -> Splay a -> (Bool, Splay a)
member x t = case search x t of
    (Leaf, []) -> (False, singleton x)
    (Leaf, bs) -> (False, splay (singleton x) bs)
    (nd, bs)   -> (True,  splay nd bs)

----------------------------------------------------------------

{-| Finding the minimum element.

>>> fst $ minimum (fromList [3,5,1])
1
>>> minimum empty
*** Exception: minimum
-}

minimum :: Splay a -> (a, Splay a)
minimum Leaf = error "minimum"
minimum t = let (x,mt) = deleteMin t in (x, Node Leaf x mt)

----------------------------------------------------------------

{-| Deleting the minimum element.

>>> snd (deleteMin (fromList [5,3,7])) == fromList [5,7]
True
>>> deleteMin empty
*** Exception: deleteMin
-}

deleteMin :: Splay a -> (a, Splay a)
deleteMin Leaf                          = error "deleteMin"
deleteMin (Node Leaf x r)               = (x,r)
deleteMin (Node (Node Leaf lx lr) x r)  = (lx, Node lr x r)
deleteMin (Node (Node ll lx lr) x r)    = let (k,mt) = deleteMin ll
                                          in (k, Node mt lx (Node lr x r))

----------------------------------------------------------------
{-| Creating a union set from two sets.

>>> union (fromList [5,3]) (fromList [5,7]) == fromList [3,5,7]
True
-}

union :: Ord a => Splay a -> Splay a -> Splay a
union Leaf t = t
union (Node a x b) t = Node (union ta a) x (union tb b)
  where
    (ta,_,tb) = undefined

----------------------------------------------------------------
-- Basic operations
----------------------------------------------------------------

{-| Checking validity of a set.
-}

valid :: Ord a => Splay a -> Bool
valid t = isOrdered t

isOrdered :: Ord a => Splay a -> Bool
isOrdered t = ordered $ toList t
  where
    ordered [] = True
    ordered [_] = True
    ordered (x:y:xys) = x < y && ordered (y:xys)


showSet :: Show a => Splay a -> String
showSet = showSet' ""

showSet' :: Show a => String -> Splay a -> String
showSet' _ Leaf = "\n"
showSet' pref (Node l x r) = show x ++ "\n"
                        ++ pref ++ "+ " ++ showSet' pref' l
                        ++ pref ++ "+ " ++ showSet' pref' r
  where
    pref' = "  " ++ pref

printSet :: Show a => Splay a -> IO ()
printSet = putStr . showSet

{-
Demo: http://www.link.cs.cmu.edu/splay/
Paper: http://www.cs.cmu.edu/~sleator/papers/self-adjusting.pdf
TopDown: http://www.cs.umbc.edu/courses/undergraduate/341/fall02/Lectures/Splay/TopDownSplay.ppt
Blog: http://chasen.org/~daiti-m/diary/?20061223
      http://www.geocities.jp/m_hiroi/clisp/clispb07.html


               fromList    minimum          delMin          member
Blanced Tree   N log N     log N            log N           log N
Skew Heap      N log N     1                log N(???)      N/A
Splay Heap     N           log N or A(N)?   log N or A(N)?  log N or A(N)?

-}
