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

import BinTree
import ListTest
import Control.Monad
import Test.Tasty.QuickCheck as QC
import Test.Tasty

instance Arbitrary Int => Arbitrary (BinTree Int) where
   arbitrary = sized arbitrarySizedTree

arbitrarySizedTree :: Int-> Gen (BinTree Int)
arbitrarySizedTree m = do
  let t = m
  l <- arbitrarySizedTree (m-1)
  r <- arbitrarySizedTree (m-1)
  elements [(Node t l r), Leaf]

prop_levelOrderTraversal :: BinTree Int -> Bool
prop_levelOrderTraversal tree = isDescending $  (extractVal . flatten) tree
  where
    flatten :: forall a. BinTree a -> [BinTree a]
    flatten t = join $ levelTraversal t
    extractVal :: forall a. [BinTree a] -> [a]
    extractVal [] = []
    extractVal (x : xs) = case x of
                          Leaf -> extractVal xs
                          Node a _ _ -> a : extractVal xs

check :: TestTree
check = testGroup "Binary Tree" [QC.testProperty "Level Order Traversal" prop_levelOrderTraversal]
