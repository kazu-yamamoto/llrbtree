module Data.Set.Splay where

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

import Data.List (foldl')
import Prelude hiding (minimum)

data Splay a = E | T (Splay a) a (Splay a)

instance Show a => Show (Splay a) where
    show = showTree

partition :: Ord a => a -> Splay a -> (Splay a, Bool, Splay a)
partition _ E = (E,False,E)
partition k x@(T xl xk xr) = case compare k xk of
    EQ -> (xl, True, xr)
    GT -> case xr of
        E -> (x, False, E)
        T yl yk yr -> case compare k yk of
            EQ ->     (T xl xk yl, True, yr)        -- right        :zig
            GT -> let (lt, b, gt) = partition k yr  -- right right  :zig zag
                  in  (T (T xl xk yl) yk lt, b, gt)
            LT -> let (lt, b, gt) = partition k yl
                  in  (T xl xk lt, b, T gt yk yr)   -- right left   :zig zig
    LT -> case xl of
        E          -> (E, False, x)
        T yl yk yr -> case compare k yk of
            EQ ->     (yl, True, T yr xk xr)        -- left         :zig
            GT -> let (lt, b, gt) = partition k yr  -- left right   :zig zag
                  in  (T yl yk lt, b, T gt xk xr)
            LT -> let (lt, b, gt) = partition k yl  -- left left    :zig zig
                  in  (lt, b, T gt yk (T yr xk xr))

empty :: Splay a
empty = E

null :: Splay a -> Bool
null E = True
null _ = False

singleton :: a -> Splay a
singleton x = T E x E

insert :: Ord a => a -> Splay a -> Splay a
insert x t = T l x r
  where
    (l,_,r) = partition x t

fromList :: Ord a => [a] -> Splay a
fromList = foldl' (flip insert) empty

member :: Ord a => a -> Splay a -> (Bool, Splay a)
member x t = if b then
                 (True, T l x r)
             else
                 (False, t)
  where
    (l,b,r) = partition x t
    
{-
  let t = fromList xs
  minimum (fromList xs) == let m = Data.List.minimum xs
                               (_, t') = member m t
                           in (m, t')
-}
minimum :: (Ord a, Bounded a) => Splay a -> (a, Splay a)
minimum t = let (x,mt) = deleteMin t in (x, T E x mt)

deleteMin :: Splay a -> (a, Splay a)
deleteMin E = error "deleteMin"
deleteMin (T E x r)            = (x,r)
deleteMin (T (T E lx lr) x r)  = (lx, T lr x r)
deleteMin (T (T ll lx lr) x r) = let (k,mt) = deleteMin ll
                                 in (k, T mt lx (T lr x r))

merge :: Ord a => Splay a -> Splay a -> Splay a
merge E t = t
merge (T a x b) t = T (merge ta a) x (merge tb b)
  where
    (ta,_,tb) = partition x t

showTree :: Show a => Splay a -> String
showTree = showTree' ""

showTree' :: Show a => String -> Splay a -> String
showTree' _ E = "\n"
showTree' pref (T l x r) = show x ++ "\n"
                        ++ pref ++ "+ " ++ showTree' pref' l
                        ++ pref ++ "+ " ++ showTree' pref' r
  where
    pref' = "  " ++ pref

printTree :: Show a => Splay a -> IO ()
printTree = putStr . showTree
