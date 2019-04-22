{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module ListTest
       (
        isDescending,
        isAscending        
        )       
where

isDescending :: (Ord a) => [a] -> Bool
isDescending a = is_Traversal (>=) a

isAscending :: (Ord a) => [a] -> Bool
isAscending a = is_Traversal (<=) a

is_Traversal :: (Ord a) => (a -> a -> Bool) -> [a] -> Bool
is_Traversal f (x:y:xs) = (f x y) && is_Traversal f (y:xs)
is_Traversal _ [] = True
is_Traversal _ [_] = True



