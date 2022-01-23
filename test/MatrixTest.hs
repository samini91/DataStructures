{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module MatrixTest
  ( 
    check,
  )
where

import Control.Monad

import Control.Monad.State
import Data.Maybe
import Data.Vector as V
import ListTest
import Matrix (Matrix(..), createMatrix, getCol, getIndex, getRow, setIndex)
import Test.Tasty
import Test.Tasty.QuickCheck as QC;
import qualified Data.Maybe as Maybe

instance Arbitrary Int => Arbitrary (Matrix Vector Int) where
  arbitrary = sized arbitraryMatrix

instance Semigroup Int where
  (<>) = (+)

instance Monoid Int where
    mempty  = 0
    mappend = (+)

arbitraryMatrix :: Int -> Gen (Matrix Vector Int)
arbitraryMatrix size = do
  rowSizePos <- arbitrary :: Gen (Positive Int)
  colSizePos <- arbitrary :: Gen (Positive Int)
  let rowSize = getPositive rowSizePos
  let colSize = getPositive colSizePos
  let matrix = createMatrix rowSize colSize
  return $ Prelude.foldl foldFunc matrix (generateTuples (rowSize, colSize))
  where
    foldFunc :: Matrix Vector Int -> (Int,Int) -> Matrix Vector Int
    foldFunc m (row,col) = setIndex m (row,col) (row+col)

generateTuples :: (Int,Int) -> [(Int,Int)]
generateTuples (rows,cols) = do
  r <- [0..rows-1]
  c <- [0..cols-1]
  return (r,c)

prop_row_times_col :: Matrix Vector Int -> Bool
prop_row_times_col matrix@(Matrix row col map) = Prelude.foldl foldFunc True (generateTuples (row, col))
  where
  foldFunc accum (r, c) = accum && maybe False (\x -> (r+c) == x ) (getIndex matrix (r, c))

check :: TestTree
check =
  testGroup
    "Matrix Test"
    [
      QC.testProperty "get index" prop_row_times_col
    ]
