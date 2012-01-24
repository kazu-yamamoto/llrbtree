{-|
  Purely functional splay heaps.

   * <http://www.cs.cmu.edu/~sleator/papers/self-adjusting.pdf>
-}

module Data.Heap.Splay (
  -- * Data structures
    Heap(..)
  , Splay(..)
  -- * Creating heaps
  , empty
  , singleton
  , insert
  , fromList
  , deleteMin
  , null
  -- * Helper functions
  , partition
  , merge
  , minimum
--  , valid
  , showHeap
  , printHeap
  ) where

import Data.List (foldl')
import Prelude hiding (minimum, maximum, null)

----------------------------------------------------------------

data Heap a = None | Some a (Splay a) deriving Show

data Splay a = Leaf | Node (Splay a) a (Splay a) deriving Show

instance (Eq a) => Eq (Splay a) where
    t1 == t2 = toList t1 == toList t2

----------------------------------------------------------------

{-| Splitting smaller and bigger with splay.
    Since this is a heap implementation, members is not
    necessarily unique.
-}
partition :: Ord a => a -> Splay a -> (Splay a, Splay a)
partition _ Leaf = (Leaf, Leaf)
partition k x@(Node xl xk xr)
  | k >= xk = case xr of
      Leaf -> (x, Leaf)
      Node yl yk yr
        | k >= yk   -> let (lt, gt) = partition k yr        -- RR :zig zag
                       in  (Node (Node xl xk yl) yk lt, gt)
        | otherwise -> let (lt, gt) = partition k yl
                       in  (Node xl xk lt, Node gt yk yr)   -- RL :zig zig
  | otherwise = case xl of
      Leaf          -> (Leaf, x)
      Node yl yk yr
        | k >= yk   -> let (lt, gt) = partition k yr        -- LR :zig zag
                       in  (Node yl yk lt, Node gt xk xr)
        | otherwise -> let (lt, gt) = partition k yl        -- LL :zig zig
                       in  (lt, Node gt yk (Node yr xk xr))

----------------------------------------------------------------

{-| Empty heap.
-}

empty :: Heap a
empty = None

{-|
See if the splay heap is empty.

>>> Data.Heap.Splay.null empty
True
>>> Data.Heap.Splay.null (singleton 1)
False
-}

null :: Heap a -> Bool
null None = True
null _    = False

{-| Singleton heap.
-}

singleton :: a -> Heap a
singleton x = Some x (Node Leaf x Leaf)

----------------------------------------------------------------

{-| Insertion.

>>> insert 5 (fromList [5,3]) == fromList [3,5]
True
>>> insert 7 (fromList [5,3]) == fromList [3,5,7]
True
>>> insert 5 empty            == singleton 5
True
-}

insert :: Ord a => a -> Heap a -> Heap a
insert x None = singleton x
insert x (Some m t) = Some m' $ Node l x r
  where
    m' = if x < m then x else m -- xxx
    (l,r) = partition x t

----------------------------------------------------------------

{-| Creating a heap from a list.

>>> empty == fromList []
True
>>> singleton 'a' == fromList ['a']
True
>>> fromList [5,3,5] == fromList [5,3]
True
-}

fromList :: Ord a => [a] -> Heap a
fromList = foldl' (flip insert) empty

----------------------------------------------------------------

{-| Creating a list from a heap. O(N)

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

{-| Finding the minimum element.

>>> fst $ minimum (fromList [3,5,1])
1
>>> minimum empty
*** Exception: minimum
-}

minimum :: Heap a -> a
minimum None       = error "minimum"
minimum (Some m _) = m

----------------------------------------------------------------

{-| Deleting the minimum element.

>>> snd (deleteMin (fromList [5,3,7])) == fromList [5,7]
True
>>> deleteMin empty
*** Exception: deleteMin
-}

deleteMin :: Heap a -> Heap a
deleteMin None       = error "deleteMin"
deleteMin (Some _ t) = case deleteMin' t of
    (_, Leaf) -> None
    (m, t')   -> Some m t'

deleteMin' :: Splay a -> (a, Splay a)
deleteMin' Leaf                          = error "deleteMin'"
deleteMin' (Node Leaf x r)               = (x,r)
deleteMin' (Node (Node Leaf lx lr) x r)  = (lx, Node lr x r)
deleteMin' (Node (Node ll lx lr) x r)    = let (k,mt) = deleteMin' ll
                                           in (k, Node mt lx (Node lr x r))

----------------------------------------------------------------
{-| Creating a union heap from two heaps.

>>> union (fromList [5,3]) (fromList [5,7]) == fromList [3,5,7]
True
-}

merge :: Ord a => Splay a -> Splay a -> Splay a
merge Leaf t = t
merge (Node a x b) t = Node (merge ta a) x (merge tb b)
  where
    (ta,tb) = partition x t

----------------------------------------------------------------
-- Basic operations
----------------------------------------------------------------

showHeap :: Show a => Splay a -> String
showHeap = showHeap' ""

showHeap' :: Show a => String -> Splay a -> String
showHeap' _ Leaf = "\n"
showHeap' pref (Node l x r) = show x ++ "\n"
                        ++ pref ++ "+ " ++ showHeap' pref' l
                        ++ pref ++ "+ " ++ showHeap' pref' r
  where
    pref' = "  " ++ pref

printHeap :: Show a => Splay a -> IO ()
printHeap = putStr . showHeap

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
