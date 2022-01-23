{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Test(main) where

import BinTree
import NNodeTree
import MinHeapTest
import Control.Monad
import ListTest
import BinTreeTest
import MatrixTest
import DijstrasTest
import Test.Tasty (defaultMain)
import Test.Tasty.Runners

prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse ys ++ reverse xs


prop_breathFirstSearchLeveled :: NNodeTree Int -> Bool
prop_breathFirstSearchLeveled  tree = isDescending $ extractVal <$> flatten tree
  where
    flatten t = join $ breathFirstSearchLeveled t
    extractVal :: NNodeTree a -> a
    extractVal (NLeaf a) = a
    extractVal (NNode a _) = a


main = defaultMain $ TestGroup "Test All" [
  MinHeapTest.check,
  MatrixTest.check,
  BinTreeTest.check,
  DijstrasTest.dijstrasTest
  ]

