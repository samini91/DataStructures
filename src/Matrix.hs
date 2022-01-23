{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE FlexibleContexts #-}

module Matrix
  (
    createMatrix,
    Matrix (..),
    getRow,
    getCol,
    setIndex,
    getIndex,
  )
where

import Data.Map as M
import Data.Vector ((!?))
import Data.Vector as V hiding (sequence)
import GHC.Generics
import Data.Maybe

class Indexable m a where
  indexOf :: m a -> Int -> Maybe a

instance Indexable V.Vector a where
  indexOf vector index = vector V.!? index

instance Indexable [] a where
  indexOf [] _ = Nothing
  indexOf (x : xs) index
    | index == 0 = Just x
    | index < 0 = Nothing
    | otherwise = indexOf xs (index -1)

class Generator m where
  generateIndex :: Int -> m Int

class (Monoid a) => GenerateSeq m a where
    generateSeq :: Int -> (Int -> a) -> m a

instance Generator [] where
  generateIndex i = [0..i]

instance Monoid a=> GenerateSeq [] a where
    generateSeq size func= func <$> [0..size-1]

instance Generator V.Vector where
  generateIndex i = V.generate i id

instance Monoid a => GenerateSeq V.Vector a where
  generateSeq = V.generate  

data Matrix m a = Matrix
  { row :: Int,
    col :: Int,
    rowMap :: M.Map Int (m a)
  } deriving (Show)


-- can strong type this to positie numbers only
type RowSize = Int
type ColSize = Int


type Row = Int
type Col = Int

createMatrix :: (Traversable m, GenerateSeq m a) => RowSize -> ColSize -> Matrix m a
createMatrix rowSize colSize = Matrix {row = rowSize, col = colSize, rowMap = createMap}
  where
    createMap = Prelude.foldl foldFunc M.empty [0..rowSize-1]
    foldFunc accum x = M.insert x (createVector colSize) accum
    createVector :: (GenerateSeq m a) => Int -> m a
    createVector size = generateSeq size (const mempty)

-- set row as well
-- setRow

-- should i return maybe? 
setIndex :: Matrix V.Vector a -> (Row, Col) -> a -> Matrix V.Vector a
setIndex matrix@(Matrix rowCnt colCnt rowMap) (row,col) val =
  let newRowMap = snd $ M.insertLookupWithKey replaceList row (V.singleton val) rowMap
  in
    if rowCnt <= row || colCnt <= col
    then
      matrix
    else
      Matrix { row = rowCnt, col= colCnt, rowMap = newRowMap }
  where
    replaceList :: Int -> V.Vector a -> V.Vector a -> V.Vector a
    replaceList key newVal oldVal = V.update oldVal ((\x -> (col,x)) <$> newVal)

getIndex :: (Traversable m, Indexable m a) => Matrix m a -> (Row, Col) -> Maybe a
getIndex matrix (row, col) = (`indexOf` col) =<< getRow row matrix

getRow :: Traversable m => Int -> Matrix m a -> Maybe (m a)
getRow row (Matrix _ _ rowMap) = M.lookup row rowMap

getCol :: (Traversable m, Indexable m a, Generator m, Monad m, Monoid a) => Int -> Matrix m a -> m a
getCol index matrix = getCols index matrix
  where
    --getRows :: (Generator m a, Monoid a) => Matrix m a -> 
    getRows ma@(Matrix rowCnt _ rowMap) = flip getRow ma <$> generateIndex rowCnt
    getCols col matrix =
      do
        z <- getRows matrix
        return $ fromMaybe mempty ((`indexOf` col) =<< z)




