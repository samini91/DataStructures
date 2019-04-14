{-# LANGUAGE FlexibleContexts #-}
module BinTree
  (
    BinTree(..),
    inOrderTraversal,
    bruteForceBinSearch,
    binSearch,
    levelTraversal
  )
  where

import Control.Applicative
data BinTree a = Node a (BinTree a) (BinTree a) | Leaf a deriving (Show)
--data BinTree a = Node a (BinTree a) (BinTree a) | Leaf a 

showVal :: (Show a) => BinTree a -> String
showVal (Leaf a) = show a
showVal (Node a _ _) = show a

-- instance Show(a) => Show (BinTree a) where
--   --show tree = show (levelTraversal tree)
--   show tree = concat $ do
--      level <- levelTraversal tree
--      return $ (concat ((\s -> (showVal s) ++ "  ") <$> level)) ++ "\n"

levelTraversal :: BinTree a -> [[BinTree a]]
levelTraversal (Leaf a) = [[Leaf a]]
levelTraversal (Node a l r) = treetraverse [[Node a l r]]
  where
    treetraverse :: [[BinTree a]] -> [[BinTree a]]
    treetraverse [] = []
    treetraverse (x:xs) = [x] ++ treetraverse(xs ++ (getChildren <$> x))
    getChildren (Node _ l r) = [l] ++ [r]
    getChildren (Leaf _) = []

    
inOrderTraversal :: BinTree a -> [a]
inOrderTraversal (Leaf a) = [a]
inOrderTraversal (Node a l r) = inOrderTraversal l ++ [a] ++ inOrderTraversal r


bruteForceBinSearch :: (Ord a) => a -> BinTree a -> Maybe (BinTree a)
bruteForceBinSearch val (Leaf a) 
  | val == a = Just (Leaf a)
  | otherwise = Nothing    

bruteForceBinSearch val (Node a l r)
  | val == a = Just (Node a l r)
  | otherwise = (bruteForceBinSearch val l) <|> (bruteForceBinSearch val r)


binSearch :: (Ord a) => a -> BinTree a -> Maybe (BinTree a)
binSearch val (Leaf a) 
  | val == a = Just (Leaf a)
  | otherwise = Nothing    

binSearch val (Node a l r)
  | val == a = Just (Node a l r)
  | otherwise = if val > a
                then
                  (bruteForceBinSearch val r)
                else
                  (bruteForceBinSearch val l)




