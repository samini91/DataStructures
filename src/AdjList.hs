{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
module AdjList where

import Data.Map as M
import Data.Maybe
import Data.Vector ((!?))
import Data.Vector as V hiding (sequence)
import GHC.Generics
import Data.Aeson

data AdjList m a = AdjList
  { edgeMap :: M.Map Int (m a) -- m should be a set to ensure uniquness
  }
  deriving (Show, Generic, Eq)

appendToList :: AdjList [] a -> Int -> a -> AdjList [] a
appendToList (AdjList rowMap) index item =
  let newRowMap = snd $ M.insertLookupWithKey append index [item] rowMap
   in AdjList {edgeMap = newRowMap}
  where
    append :: Int -> [a] -> [a] -> [a]
    append _ toAppend oldList = oldList Prelude.++ toAppend

replaceList :: AdjList [] a -> Int -> [a] -> AdjList [] a
replaceList (AdjList rowMap) index item =
  let newRowMap = snd $ M.insertLookupWithKey append index item rowMap
   in AdjList {edgeMap = newRowMap}
  where
    append :: Int -> [a] -> [a] -> [a]
    append _ toReplace _ = toReplace

getAdj :: AdjList [] a -> Int -> [a]
getAdj (AdjList rowMap) index = fromMaybe [] (M.lookup index rowMap)
