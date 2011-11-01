module Data.RBTree.Internal where

----------------------------------------------------------------

data RBTree a = Leaf -- color is Black
              | Node Color !BlackHeight !(RBTree a) a !(RBTree a)
              deriving (Eq,Show)

data Color = B | R deriving (Eq,Show)
type BlackHeight = Int

----------------------------------------------------------------

height :: RBTree a -> BlackHeight
height Leaf = 0
height (Node _ h _ _ _) = h

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
    blacks' n (Node R _ l _ r) = blacks' n  l ++ blacks' n  r
    blacks' n (Node B _ l _ r) = blacks' n' l ++ blacks' n' r
      where
        n' = n + 1

isRedSeparate :: RBTree a -> Bool
isRedSeparate = reds B

reds :: Color -> RBTree t -> Bool
reds _ Leaf = True
reds R (Node R _ _ _ _) = False
reds _ (Node c _ l _ r) = reds c l && reds c r


blackHeight :: RBTree a -> Bool
blackHeight Leaf = True
blackHeight t@(Node B i _ _ _) = bh i t
  where
    bh n Leaf = n == 0
    bh n (Node R h l _ r) = n == h' && bh n l && bh n r
      where
        h' = h - 1
    bh n (Node B h l _ r) = n == h && bh n' l && bh n' r
      where
        n' = n - 1
blackHeight _ = error "blackHeight"

----------------------------------------------------------------

member :: Ord a => a -> RBTree a -> Bool
member _ Leaf = False
member x (Node _ _ l y r) = case compare x y of
    LT -> member x l
    GT -> member x r
    EQ -> True

----------------------------------------------------------------

toList :: RBTree a -> [a]
toList t = inorder t []
  where
    inorder Leaf xs = xs
    inorder (Node _ _ l x r) xs = inorder l (x : inorder r xs)

----------------------------------------------------------------

turnR :: RBTree a -> RBTree a
turnR Leaf             = error "turnR"
turnR (Node _ h l x r) = Node R h l x r

turnB :: RBTree a -> RBTree a
turnB Leaf           = error "turnB"
turnB (Node _ h l x r) = Node B h l x r

turnB' :: RBTree a -> RBTree a
turnB' Leaf             = Leaf
turnB' (Node _ h l x r) = Node B h l x r

----------------------------------------------------------------

isBlack :: RBTree a -> Bool
isBlack Leaf            = True
isBlack (Node B _ _ _ _ ) = True
isBlack _               = False

isRed :: RBTree a -> Bool
isRed (Node R _ _ _ _ ) = True
isRed _               = False


----------------------------------------------------------------

showTree :: Show a => RBTree a -> String
showTree = showTree' ""

showTree' :: Show a => String -> RBTree a -> String
showTree' _ Leaf = "\n"
showTree' pref (Node k h l x r) = show k ++ " " ++ show x ++ " (" ++ show h ++ ")\n"
                               ++ pref ++ "+ " ++ showTree' pref' l
                               ++ pref ++ "+ " ++ showTree' pref' r
  where
    pref' = "  " ++ pref
