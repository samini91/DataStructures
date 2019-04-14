{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.All
import BinTree
import NNodeTree
import Control.Monad

main = quickCheck prop_levelOrderTraversal


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

prop_breathFirstSearchLeveled :: (NNodeTree Int) -> Bool
prop_breathFirstSearchLeveled  tree = isDescending $ (extractVal) <$> flatten tree
  where
    flatten t = (join $ breathFirstSearchLeveled t) 
    extractVal :: NNodeTree a -> a
    extractVal (Leaf a) = a
    extractVal (Node a _) = a

isDescending :: (Ord a) => [a] -> Bool
isDescending (x:y:xs) = (x >= y) && isDescending(y:xs)
isDescending [] = True
isDescending [_] = True

--main = return quickCheckAll -- not sure how im supposed to use this


--main = putStrLn (x >>= show)
-- main = do
--    tree <- x
--    putStrLn $ "\n" ++show tree
