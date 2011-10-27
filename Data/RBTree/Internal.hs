module Data.RBTree.Internal where

----------------------------------------------------------------

data RBTree a = Leaf -- color is Black
              | Fork Color !(RBTree a) a !(RBTree a) deriving (Eq,Show)

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

----------------------------------------------------------------

turnR :: RBTree a -> RBTree a
turnR Leaf           = error "turnR"
turnR (Fork _ l x r) = Fork R l x r

turnB :: RBTree a -> RBTree a
turnB Leaf           = error "turnB"
turnB (Fork _ l x r) = Fork B l x r

----------------------------------------------------------------

isBlack :: RBTree a -> Bool
isBlack Leaf            = True
isBlack (Fork B _ _ _ ) = True
isBlack _               = False

isRed :: RBTree a -> Bool
isRed (Fork R _ _ _ ) = True
isRed _               = False

----------------------------------------------------------------

isBlackLeftBlack :: RBTree a -> Bool
isBlackLeftBlack (Fork B l _ _)
  | isBlack l      = True
isBlackLeftBlack _ = False

isBlackLeftRed :: RBTree a -> Bool
isBlackLeftRed (Fork B l _ _)
  | isRed l        = True
isBlackLeftRed _   = False

----------------------------------------------------------------

left :: RBTree a -> RBTree a
left (Fork _ l _ _ ) = l
left _               = error "left"
