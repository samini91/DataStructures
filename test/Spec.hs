{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.All
import BinTree
import NNodeTree
import MinHeapTest
import Control.Monad
import ListTest
import BinTreeTest

prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse ys ++ reverse xs

-- An unbound arbritrary implementation
-- instance Arbitrary a => Arbitrary (BinTree a) where
--   arbitrary = arbitrarySizedTree

-- arbitrarySizedTree :: forall a. (Arbitrary a) => Gen (BinTree a)
-- arbitrarySizedTree = do
--   t <- arbitrary
--   l <- arbitrarySizedTree 
--   r <- arbitrarySizedTree 
--   z <- elements [(Node t l r), (Leaf t)]
--   return z
prop_breathFirstSearchLeveled :: (NNodeTree Int) -> Bool
prop_breathFirstSearchLeveled  tree = isDescending $ (extractVal) <$> flatten tree
  where
    flatten t = (join $ breathFirstSearchLeveled t) 
    extractVal :: NNodeTree a -> a
    extractVal (NLeaf a) = a
    extractVal (NNode a _) = a

main = do
       MinHeapTest.check
       BinTreeTest.check 
     
       

