{-|
  Purely functional splay trees.

   * <http://www.cs.cmu.edu/~sleator/papers/self-adjusting.pdf>
-}

module Data.Set.Splay (
  -- * Data structures
    Splay(..)
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
  , partition
--  , merge
  , minimum
--  , maximum
  , valid
  , showTree
  , printTree
  ) where

import Data.List (foldl')
import Prelude hiding (minimum, maximum, null)

----------------------------------------------------------------

data Splay a = Leaf | Node (Splay a) a (Splay a) deriving Show

instance (Eq a) => Eq (Splay a) where
    t1 == t2 = toList t1 == toList t2

----------------------------------------------------------------

{-| Splitting smaller and bigger with splay.
    Since this is a set implementation, members must be unique.
-}
partition :: Ord a => a -> Splay a -> (Splay a, Bool, Splay a)
partition _ Leaf = (Leaf,False,Leaf)
partition k x@(Node xl xk xr) = case compare k xk of
    EQ -> (xl, True, xr)
    GT -> case xr of
        Leaf -> (x, False, Leaf)
        Node yl yk yr -> case compare k yk of
            EQ ->     (Node xl xk yl, True, yr)           -- R  :zig
            GT -> let (lt, b, gt) = partition k yr        -- RR :zig zag
                  in  (Node (Node xl xk yl) yk lt, b, gt)
            LT -> let (lt, b, gt) = partition k yl
                  in  (Node xl xk lt, b, Node gt yk yr)   -- RL :zig zig
    LT -> case xl of
        Leaf          -> (Leaf, False, x)
        Node yl yk yr -> case compare k yk of
            EQ ->     (yl, True, Node yr xk xr)           -- L  :zig
            GT -> let (lt, b, gt) = partition k yr        -- LR :zig zag
                  in  (Node yl yk lt, b, Node gt xk xr)
            LT -> let (lt, b, gt) = partition k yl        -- LL :zig zig
                  in  (lt, b, Node gt yk (Node yr xk xr))

----------------------------------------------------------------

{-| Empty tree.
-}

empty :: Splay a
empty = Leaf

{-|
See if the splay tree is empty.

>>> Data.Set.Splay.null empty
True
>>> Data.Set.Splay.null (singleton 1)
False
-}

null :: Splay a -> Bool
null Leaf = True
null _ = False

{-| Singleton tree.
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
insert x t = Node l x r
  where
    (l,_,r) = partition x t

----------------------------------------------------------------

{-| Creating a tree from a list.

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

{-| Creating a list from a tree. O(N)

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

{-| Checking if this element is a member of a tree?

>>> fst $ member 5 (fromList [5,3])
True
>>> fst $ member 1 (fromList [5,3])
False
-}

member :: Ord a => a -> Splay a -> (Bool, Splay a)
member x t = if b then
                 (True, Node l x r)
             else
                 (False, t)
  where
    (l,b,r) = partition x t

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
deleteMin Leaf = error "deleteMin"
deleteMin (Node Leaf x r)            = (x,r)
deleteMin (Node (Node Leaf lx lr) x r)  = (lx, Node lr x r)
deleteMin (Node (Node ll lx lr) x r) = let (k,mt) = deleteMin ll
                                 in (k, Node mt lx (Node lr x r))

----------------------------------------------------------------
{-| Creating a union tree from two trees.

>>> union (fromList [5,3]) (fromList [5,7]) == fromList [3,5,7]
True
-}

union :: Ord a => Splay a -> Splay a -> Splay a
union Leaf t = t
union (Node a x b) t = Node (union ta a) x (union tb b)
  where
    (ta,_,tb) = partition x t

----------------------------------------------------------------
-- Basic operations
----------------------------------------------------------------

{-| Checking validity of a tree.
-}

valid :: Ord a => Splay a -> Bool
valid t = isOrdered t

isOrdered :: Ord a => Splay a -> Bool
isOrdered t = ordered $ toList t
  where
    ordered [] = True
    ordered [_] = True
    ordered (x:y:xys) = x < y && ordered (y:xys)


showTree :: Show a => Splay a -> String
showTree = showTree' ""

showTree' :: Show a => String -> Splay a -> String
showTree' _ Leaf = "\n"
showTree' pref (Node l x r) = show x ++ "\n"
                        ++ pref ++ "+ " ++ showTree' pref' l
                        ++ pref ++ "+ " ++ showTree' pref' r
  where
    pref' = "  " ++ pref

printTree :: Show a => Splay a -> IO ()
printTree = putStr . showTree

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
