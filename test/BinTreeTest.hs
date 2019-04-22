{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module BinTreeTest
(
        prop_levelOrderTraversal,
        check
)
where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import BinTree
import ListTest
import Control.Monad

instance Arbitrary Int => Arbitrary (BinTree Int) where
   arbitrary = sized arbitrarySizedTree

arbitrarySizedTree :: Int-> Gen (BinTree Int)
arbitrarySizedTree m = do
  t <- return m
  l <- arbitrarySizedTree (m-1)
  r <- arbitrarySizedTree (m-1)
  z <- elements [(Node t l r), (Leaf t)]
  return z

prop_levelOrderTraversal :: (BinTree Int) -> Bool
prop_levelOrderTraversal tree = isDescending $ (extractVal) <$> flatten tree
  where
    flatten t = (join $ levelTraversal t) 
    extractVal :: BinTree a -> a -- there must be a better way to extract values from data...
    extractVal (Leaf a) = a
    extractVal (Node a _ _) = a


return []
check = $quickCheckAll


