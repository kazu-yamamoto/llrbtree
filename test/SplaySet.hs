{-# LANGUAGE CPP, TemplateHaskell #-}

module Main where

#if METHOD == 1
import Data.Set.BUSplay
#else
import Data.Set.Splay
#endif
import Prelude hiding (minimum, maximum)

import Test.Framework.TH.Prime
import Test.Framework.Providers.DocTest.Prime
import Test.Framework.Providers.HUnit
import Test.HUnit

main :: IO ()
main = $(defaultMainGenerator)

----------------------------------------------------------------

doc_test :: DocTest
#if METHOD == 1
doc_test = docTest ["../Data/Set/BUSplay.hs"] ["-i.."]
#else
doc_test = docTest ["../Data/Set/Splay.hs"] ["-i.."]
#endif

----------------------------------------------------------------

(@?==) :: Eq a => Splay a -> Splay a -> Assertion
t @?== a = assertBool "" $ t === a

----------------------------------------------------------------

t_splay :: Splay Int
t_splay = i
  where
    a = Node Leaf 5 Leaf
    b = Node Leaf 4 a
    c = Node Leaf 3 b
    d = Node Leaf 2 c
    e = Node d    6 Leaf
    f = Node Leaf 1 e
    g = Node f    7 Leaf
    h = Node g    8 Leaf
    i = Node h    9 Leaf

a_splay :: Splay Int
a_splay = a
  where
    c = Node Leaf 3 Leaf
    b = Node c    4 Leaf
    d = Node Leaf 2 b
    f = Node Leaf 1 d
    e = Node Leaf 6 Leaf
    g = Node e    7 Leaf
    i = Node Leaf 9 Leaf
    h = Node g    8 i
    a = Node f    5 h

case_splay :: Assertion
case_splay = snd (member 5 t_splay) @?== a_splay

----------------------------------------------------------------

t_zigzig :: Splay Int
t_zigzig = g
  where
    a = Node Leaf 1 Leaf
    b = Node a    2 Leaf
    c = Node b    3 Leaf
    d = Node c    4 Leaf
    e = Node d    5 Leaf
    f = Node e    6 Leaf
    g = Node f    7 Leaf

a_zigzig :: Splay Int
a_zigzig = a
  where
    c = Node Leaf 3 Leaf
    b = Node Leaf 2 c
    e = Node Leaf 5 Leaf
    d = Node b    4 e
    g = Node Leaf 7 Leaf
    f = Node d    6 g
    a = Node Leaf 1 f

case_zigzig :: Assertion
case_zigzig = snd (member 1 t_zigzig) @?== a_zigzig

----------------------------------------------------------------

t_zigzig_zig :: Splay Int
t_zigzig_zig = l
  where
    Node l _ _ = t_zigzig

a_zigzig_zig :: Splay Int
a_zigzig_zig = a
  where
#if METHOD == 1
    c = Node Leaf 3 Leaf
    b = Node Leaf 2 c
    e = Node Leaf 5 Leaf
    d = Node b    4 e
    f = Node d    6 Leaf
    a = Node Leaf 1 f
#else
    b = Node Leaf 2 Leaf
    d = Node Leaf 4 Leaf
    c = Node b    3 d
    f = Node Leaf 6 Leaf
    e = Node c    5 f
    a = Node Leaf 1 e
#endif

case_zigzig_zig :: Assertion
case_zigzig_zig = snd (member 1 t_zigzig_zig) @?== a_zigzig_zig

----------------------------------------------------------------

t_zigzag :: Splay Int
t_zigzag = g
  where
    a = Node Leaf 4 Leaf
    b = Node Leaf 3 a
    c = Node b    5 Leaf
    d = Node Leaf 2 c
    e = Node d    6 Leaf
    f = Node Leaf 1 e
    g = Node f    7 Leaf

a_zigzag :: Splay Int
a_zigzag = a
  where
    b = Node Leaf 3 Leaf
    d = Node Leaf 2 b
    f = Node Leaf 1 d
    c = Node Leaf 5 Leaf
    e = Node c    6 Leaf
    g = Node e    7 Leaf
    a = Node f    4 g

case_zigzag :: Assertion
case_zigzag = snd (member 4 t_zigzag) @?== a_zigzag

----------------------------------------------------------------

t_zigzag_zig :: Splay Int
t_zigzag_zig = l
  where
    Node l _ _ = t_zigzag

-- Surprise: TD and BU is the same!
a_zigzag_zig :: Splay Int
a_zigzag_zig = a
  where
    b = Node Leaf 3 Leaf
    d = Node Leaf 2 b
    f = Node Leaf 1 d
    c = Node Leaf 5 Leaf
    e = Node c    6 Leaf
    a = Node f    4 e

case_zigzag_zig :: Assertion
case_zigzag_zig = snd (member 4 t_zigzag_zig) @?== a_zigzag_zig
