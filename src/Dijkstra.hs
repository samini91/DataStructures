{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Dijkstra (EdgeList (..), openDijkstrasJSON, NodeAndWeight(..), process, NodeWithLowestWeight(..)) where

import AdjList
import Data.Aeson
import Data.Map as M
import Data.Maybe
import Data.Set as S
import Data.Vector as V hiding (minimumBy, concat)
import Data.List as L ((++), concat, minimumBy, filter, foldl)
import GHC.Generics
import MinHeap (MinHeap (Leaf), insert', maybePop)
import Data.Function (on)
import Debug.Trace (trace, traceShow)

type Weight = Int
type Vertex = Int

newtype EdgeList = EdgeList {edges :: V.Vector (V.Vector Int)}
  deriving (Generic, Show, Eq)

instance ToJSON EdgeList
instance FromJSON EdgeList

openDijkstrasJSON :: IO String
openDijkstrasJSON = readFile "./DijstrasTest.json"

type Origin = NodeWithTotalWeightToNode
type Dest = NodeAndWeight

--type MinWeight = Int
newtype TotalMinWeightToNode = TotalMinWeightToNode {totalMinWeight :: Int} deriving (Show, Eq, Generic, Ord)

--data FromNode = Int deriving (Eq, Generic, Ord)
type FromNode = Int

type Visited = M.Map Int (TotalMinWeightToNode, FromNode)
type LowestTotalWeightHeap = MinHeap (NodeAndWeight, FromNode)

newtype NextIteration = NextIteration {nodeAndWeight :: [NodeAndWeight]}

data NodeAndWeight = NodeAndWeight {node :: Int, weight :: Int} deriving (Eq, Generic, Show)

instance Ord NodeAndWeight where
  compare a b 
    | weight a > weight b = GT
    | weight a < weight b = LT
    | weight a == weight b = EQ 

newtype NodeWithLowestWeight = NodeWithLowestWeight {nodeWithWeight :: NodeAndWeight} deriving (Show, Eq, Generic, Ord)
data NodeWithTotalWeightToNode = NodeWithTotalWeightToNode {totalWeight_nodeNum :: Int, totalWeight_weight :: TotalMinWeightToNode} deriving (Show, Eq, Generic, Ord)

-- look into lenses for this
nodeWithTotalWeightToNode_NodeWithLowestWeight:: NodeWithTotalWeightToNode -> NodeWithLowestWeight
nodeWithTotalWeightToNode_NodeWithLowestWeight (NodeWithTotalWeightToNode nodeNum (TotalMinWeightToNode weight)) = NodeWithLowestWeight $ NodeAndWeight nodeNum weight

process :: Int -> Int -> AdjList [] NodeAndWeight -> [NodeWithLowestWeight]
process origin = process' (NodeWithTotalWeightToNode origin (TotalMinWeightToNode 0))

process' :: Origin -> Int -> AdjList [] NodeAndWeight -> [NodeWithLowestWeight]
process' origin@(NodeWithTotalWeightToNode nodeNum (TotalMinWeightToNode weight)) dest adjList =
  let (heap, finalVisited) = doSteps dest (insert' (NodeAndWeight nodeNum weight, nodeNum) Leaf) adjList M.empty
  in
    traceShow heap
    traceShow finalVisited
    findMinPath origin dest finalVisited
  where
    doSteps :: Int -> LowestTotalWeightHeap -> AdjList [] NodeAndWeight -> Visited -> (LowestTotalWeightHeap, Visited)
    doSteps dest minHeap adjList visited =
      let (h, v) = step minHeap adjList visited
          s = M.lookup dest v -- what if dest is never in visited ie. bad dest input?
      in
        case s of
          Nothing -> doSteps dest h adjList v
          Just a -> (h, v)

step :: LowestTotalWeightHeap -> AdjList [] NodeAndWeight -> Visited -> (LowestTotalWeightHeap, Visited)
step minHeap adjList visited =
  let (n, h) = getFirstUnvisitedFromHeap minHeap visited
      newVisited = case n of
        Nothing -> visited
        Just (s, from) -> M.insert (node s) (TotalMinWeightToNode $ weight s, from) visited 
      newHeap = do
       (nodeAndWeight, fromNode) <- n
       let totalWeight = nodeAndWeight `getAdjForNodeAndAddTotalWeight` adjList
       return $ addUnvisitedToMinHeap (node nodeAndWeight) totalWeight newVisited h
  in
    (fromMaybe Leaf newHeap, newVisited)
  where
    getFirstUnvisitedFromHeap :: LowestTotalWeightHeap -> Visited -> (Maybe (NodeAndWeight, FromNode), LowestTotalWeightHeap)
    getFirstUnvisitedFromHeap Leaf _ = (Nothing, Leaf)
    getFirstUnvisitedFromHeap minHeap visited = 
      do
        let (x, h) =  maybePop minHeap
        case x of
          Nothing -> (Nothing, h)
          Just poppedNode@(NodeAndWeight nodeNum _, fromNode) ->
            case M.lookup (node $ fst poppedNode) visited of
              Nothing -> (return poppedNode, h)
              Just _ -> getFirstUnvisitedFromHeap h visited


getAdjForNodeAndAddTotalWeight :: NodeAndWeight -> AdjList [] NodeAndWeight -> [NodeAndWeight]
getAdjForNodeAndAddTotalWeight node@(NodeAndWeight nodeNum weight) adjList = (`addWeight` weight) <$> getAdj adjList nodeNum
  where
    addWeight :: NodeAndWeight -> Int -> NodeAndWeight
    addWeight n@(NodeAndWeight nodeNum weight) w = NodeAndWeight nodeNum (weight+w)


addUnvisitedToMinHeap :: FromNode -> [NodeAndWeight] -> Visited -> LowestTotalWeightHeap -> LowestTotalWeightHeap
addUnvisitedToMinHeap fromNode adjListRes visited minHeap = addToMinHeap fromNode (getUnVisited adjListRes visited) minHeap
  where
    getUnVisited :: [NodeAndWeight] -> Visited -> [NodeAndWeight]
    getUnVisited nodeAndWeights visited = L.filter (\x -> isNothing $ M.lookup (node x) visited) nodeAndWeights

    addToMinHeap :: FromNode -> [NodeAndWeight] -> LowestTotalWeightHeap -> LowestTotalWeightHeap
    addToMinHeap fromNode unVisited minHeap = L.foldl (flip insert') minHeap ((\x -> (x, fromNode)) <$> unVisited)


findMinPath :: Origin -> Int -> Visited -> [NodeWithLowestWeight]
findMinPath origin dest visited =
  do
    (minWeight, fromNode) <- maybeToList $ M.lookup dest visited
    if dest == totalWeight_nodeNum origin
      then [nodeWithTotalWeightToNode_NodeWithLowestWeight origin]
    else findMinPath origin fromNode visited L.++ [NodeWithLowestWeight $ NodeAndWeight dest (totalMinWeight minWeight)] -- right associated concat ... bad performance here

