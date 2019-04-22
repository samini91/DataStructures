{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module MinHeapTest
(
   minHeap,
   prop_MinHeap,
   check
)
where 
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import MinHeap
import Control.Monad.State
import ListTest

instance Arbitrary Int => Arbitrary (MinHeap Int) where
         arbitrary = sized arbitraryMinHeap

-- should probably try this with the statemonad ... probably wont be better to look at tho 
arbitraryMinHeap :: Int -> Gen (MinHeap Int) 
arbitraryMinHeap size = do
                 m <- arbitrary :: Gen (Int)
                 heap <- arbitraryMinHeap (size-1)
                 if(size /= 0)
                 then
                  return $ insert' m heap
                 else
                  return $ Leaf

prop_MinHeap :: (MinHeap Int) -> Bool
prop_MinHeap heap = (isAscending . popAll) heap


x :: MinHeap a
x = Leaf
--minHeap = merge (lheap minHeap3) (rheap minHeap3)
--         where
--         lheap (Node _ _ l _) = l
--         rheap (Node _ _ _ r) = r

minHeapState :: State (MinHeap Int) ()
minHeapState = do
             insert 3
             insert 1 
             insert 4
             insert 343
             insert 0
             insert 23
             insert 4848
             insert (-33)

minHeapState' :: State (MinHeap Int) ()
minHeapState' = do
                insert (-333)
                insert (-333333)
                insert (-33)

minHeap0 = runState minHeapState x
minHeap1= runState minHeapState' x

--minHeap = merge (snd minHeap0) (snd minHeap1)
minHeap = popAll $ merge (snd minHeap0) (snd minHeap1)
            
return []
check = $quickCheckAll          

                




