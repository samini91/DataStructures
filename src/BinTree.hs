{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
module BinTree
  (
    BinTree(..),
    inOrderTraversalPrime,
    bruteForceBinSearch,
    binSearch,
    levelTraversal
  )
  where

import Control.Applicative

data BinTree a = Node a (BinTree a) (BinTree a) | Leaf deriving (Show, Functor)
--data BinTree a = Node a (BinTree a) (BinTree a) | Leaf a 

showVal :: (Show a) => BinTree a -> String
showVal Leaf = ""
showVal (Node a _ _) = show a

-- instance Show(a) => Show (BinTree a) where
--   --show tree = show (levelTraversal tree)
--   show tree = concat $ do
--      level <- levelTraversal tree
--      return $ (concat ((\s -> (showVal s) ++ "  ") <$> level)) ++ "\n"

instance Foldable BinTree where
  foldMap f Leaf = mempty
  foldMap f (Node v l r) = f v `mappend` foldMap f l `mappend` foldMap f r 


levelTraversal :: BinTree a -> [[BinTree a]]
levelTraversal Leaf = [[Leaf]]
levelTraversal (Node a l r) = treetraverse [[Node a l r]]
  where
    treetraverse :: [[BinTree a]] -> [[BinTree a]]
    treetraverse [] = []
    treetraverse (x:xs) = x : treetraverse(xs ++ (getChildren <$> x))
    getChildren (Node _ l r) =  l : [r]
    getChildren Leaf = []


inOrderTraversalPrime :: BinTree a -> [a]
inOrderTraversalPrime  Leaf = []
inOrderTraversalPrime (Node a l r) = inOrderTraversalPrime l ++ [a] ++ inOrderTraversalPrime r


bruteForceBinSearch :: (Ord a) => a -> BinTree a -> Maybe (BinTree a)
bruteForceBinSearch val Leaf = Nothing
bruteForceBinSearch val (Node a l r)
  | val == a = Just (Node a l r)
  | otherwise = bruteForceBinSearch val l <|> bruteForceBinSearch val r


binSearch :: (Ord a) => a -> BinTree a -> Maybe (BinTree a)
binSearch val Leaf = Nothing
binSearch val (Node a l r)
  | val == a = Just (Node a l r)
  | otherwise = if val > a
                then
                  bruteForceBinSearch val r
                else
                  bruteForceBinSearch val l






