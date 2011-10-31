module Data.RBTree.Internal where

----------------------------------------------------------------

data RBTree a = Leaf -- color is Black
              | Node Color !(RBTree a) a !(RBTree a) deriving (Eq,Show)

data Color = B | R deriving (Eq,Show)

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
    blacks' n (Node R l _ r) = blacks' n  l ++ blacks' n  r
    blacks' n (Node B l _ r) = blacks' n' l ++ blacks' n' r
      where
        n' = n + 1

isRedSeparate :: RBTree a -> Bool
isRedSeparate = reds B

reds :: Color -> RBTree t -> Bool
reds _ Leaf = True
reds R (Node R _ _ _) = False
reds _ (Node c l _ r) = reds c l && reds c r

----------------------------------------------------------------

member :: Ord a => a -> RBTree a -> Bool
member _ Leaf = False
member x (Node _ l y r) = case compare x y of
    LT -> member x l
    GT -> member x r
    EQ -> True

----------------------------------------------------------------

toList :: RBTree a -> [a]
toList t = inorder t []
  where
    inorder Leaf xs = xs
    inorder (Node _ l x r) xs = inorder l (x : inorder r xs)

----------------------------------------------------------------

turnR :: RBTree a -> RBTree a
turnR Leaf           = error "turnR"
turnR (Node _ l x r) = Node R l x r

turnB :: RBTree a -> RBTree a
turnB Leaf           = error "turnB"
turnB (Node _ l x r) = Node B l x r

----------------------------------------------------------------

isBlack :: RBTree a -> Bool
isBlack Leaf            = True
isBlack (Node B _ _ _ ) = True
isBlack _               = False

isRed :: RBTree a -> Bool
isRed (Node R _ _ _ ) = True
isRed _               = False

----------------------------------------------------------------

showTree :: Show a => RBTree a -> String
showTree = showTree' ""

showTree' :: Show a => String -> RBTree a -> String
showTree' _ Leaf = "\n"
showTree' pref (Node k l x r) = show k ++ " " ++ show x ++ "\n"
                               ++ pref ++ "+ " ++ showTree' pref' l
                               ++ pref ++ "+ " ++ showTree' pref' r
  where
    pref' = "  " ++ pref
