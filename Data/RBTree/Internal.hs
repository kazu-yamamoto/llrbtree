module Data.RBTree.Internal where

----------------------------------------------------------------

data RBTree a = Leaf -- color is Black
              | Fork Color !(RBTree a) a !(RBTree a) deriving (Eq,Show)

data Color = R | B deriving (Eq,Show)

----------------------------------------------------------------

empty :: RBTree a
empty = Leaf

----------------------------------------------------------------

isBalanced :: RBTree a -> Bool
isBalanced t = isBlackSame t && isRedSeparate t

isBlackSame :: RBTree a -> Bool
isBlackSame t = all (n==) ns
  where
    n:ns = blacks t

blacks :: RBTree a -> [Int]
blacks = blacks' 0
  where
    blacks' n Leaf = [n+1]
    blacks' n (Fork R l _ r) = blacks' n  l ++ blacks' n  r
    blacks' n (Fork B l _ r) = blacks' n' l ++ blacks' n' r
      where
        n' = n + 1

isRedSeparate :: RBTree a -> Bool
isRedSeparate t = reds B t

reds :: Color -> RBTree t -> Bool
reds _ Leaf = True
reds R (Fork R _ _ _) = False
reds _ (Fork c l _ r) = reds c l && reds c r

----------------------------------------------------------------

member :: Ord a => a -> RBTree a -> Bool
member _ Leaf = False
member x (Fork _ l y r) = case compare x y of
    LT -> member x l
    GT -> member x r
    EQ -> True

----------------------------------------------------------------

toList :: RBTree a -> [a]
toList t = inorder t []
  where
    inorder Leaf xs = xs
    inorder (Fork _ l x r) xs = inorder l (x : inorder r xs)
