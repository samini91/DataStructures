-- Based on the leftist heap 
module MinHeap
  (
  MinHeap(..),
  getRank,
  merge,
  insert,
  insert',
  pop,
  popAll
  )
  where

import Control.Monad.State
-- The int value is the distance of the right most node in a given tree.
-- We should save that value rather than traverse the tree to figure it out everytime
data MinHeap a = Leaf | Node Int a (MinHeap a) (MinHeap a) deriving (Show)

getRank :: MinHeap a -> Rank
getRank (Node rank _ _ _) = rank
getRank Leaf = 0

insert :: (Ord a) => a -> State (MinHeap a) ()
insert val = state $ (\heap -> ((), insert' val heap ))

insert' :: (Ord a) => a -> MinHeap a-> (MinHeap a)
insert' val heap = merge (Node 0 val Leaf Leaf) heap

type Rank = Int
-- if left is bigger than right then merge left into right subtree
-- signifies that if the left tree has a value greater than the right
-- i need to swap if the ranks are not the same length
merge :: (Ord a) => MinHeap a -> MinHeap a -> (MinHeap a)
merge l@(Node _ lVal leftL leftR) r@(Node _ rVal _ _)
      | lVal <= rVal = 
      let node = (merge leftR r) in
      if getRank node <= getRank leftL -- do we need to swap? if greater than leftL Ranks then swap
      then (Node (getRank node + 1) lVal leftL node) -- no swap
      else (Node (getRank leftL) lVal node leftL) -- swap also need to swap ranks
      | otherwise = merge r l -- swap the arguments and recurse

-- Base Cases
merge (Leaf) (Node rRank rVal rightL rightR) = (Node (rRank) rVal rightL rightR)
merge (Node lRank lVal leftL leftR) (Leaf)  = (Node lRank lVal leftL leftR)
merge (Leaf) (Leaf) = (Leaf)

pop :: State (MinHeap Int) (Int)
pop = state $ (\heap -> (pop' heap))

pop' :: MinHeap Int -> (Int, MinHeap Int)
pop' (Node _ val l r ) = (val,(merge l r))
pop' (Leaf) = (0, Leaf) -- hmm this is a problem leaf doenst have a value so what do i return ? do i put an a in the constructor or lift this in a monad? 

popAll :: (MinHeap Int) -> [Int]
popAll heap@(Node _ _ _ _) = let (p,h) = (pop' heap) in p : popAll h
popAll Leaf = []















