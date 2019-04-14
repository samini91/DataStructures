module NNodeTree
  (
    NNodeTree(..),
    depthFirstSearch,
    breathFirstSearch,
    breathFirstSearchLeveled
  )
  where

data NNodeTree a = NNode a [(NNodeTree a)] | NLeaf a

depthFirstSearch :: NNodeTree a -> [a]
depthFirstSearch (NLeaf a) = [a]
depthFirstSearch (NNode a lst) = [a] ++ (lst >>= depthFirstSearch)

breathFirstSearch :: NNodeTree a -> [NNodeTree a]
breathFirstSearch (NLeaf a) = [NLeaf a]
breathFirstSearch (NNode a lst) = traverseTree [(NNode a lst)]
  where
    traverseTree :: [NNodeTree a] -> [NNodeTree a]
    traverseTree (x:xs) = [x] ++ traverseTree (xs ++ getChildren x)
    traverseTree [] = []
    getChildren (NNode _ l) = l
    getChildren (NLeaf x) = [NLeaf x]

breathFirstSearchLeveled :: NNodeTree a -> [[NNodeTree a]]
breathFirstSearchLeveled (NLeaf a) = [[NLeaf a]]
breathFirstSearchLeveled (NNode a lst) = traverseTree [[(NNode a lst)]]
  where
    traverseTree :: [[NNodeTree a]] -> [[NNodeTree a]]
    traverseTree (x:xs) = x : traverseTree (filter isEmptyList $ xs ++ (return $ x >>= getChildren))
    traverseTree [] = []
    isEmptyList [] = True
    isEmptyList (_:_) = False
    getChildren (NNode _ l) = l
    getChildren (NLeaf _) = []





