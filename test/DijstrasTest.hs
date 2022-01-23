module DijstrasTest

where

import Test.Tasty
import Data.Vector as V
import Test.Tasty.HUnit
import Dijkstra (openDijkstrasJSON, EdgeList(..))
import Data.Aeson (decode)
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy.UTF8 as BLU
import AdjList
import Data.Map as M
import Dijkstra (NodeAndWeight(..), process, NodeWithLowestWeight(..))

dijstrasTest :: TestTree
--dijstrasTest = testGroup "DijstrasTest" [openFileTest, wikipediaExample]
dijstrasTest = testGroup "DijstrasTest" [dijstras]

openFileTest :: TestTree
openFileTest = testGroup "JSON"
  [
    testCase "Open File" $ do
      str <- openDijkstrasJSON
      assertEqual
        ""
        ( Just
            EdgeList
              { edges =
                  V.singleton $ V.fromList [1, 100, 20]
              }
        )
        (dijstraFromFile str)
  ]
  where
    dijstraFromFile :: String -> Maybe EdgeList
    dijstraFromFile = decode . BLU.fromString


dijstras :: TestTree
dijstras = testGroup "Run Dijkstra"
  [
     testCase "Example in wikipedia" $ 
     assertEqual
       "Computes correct result"
       [ NodeWithLowestWeight $ NodeAndWeight 1 0,
         NodeWithLowestWeight $ NodeAndWeight 3 9,
         NodeWithLowestWeight $ NodeAndWeight 6 11,
         NodeWithLowestWeight $ NodeAndWeight 5 20
       ]
       (process 1 5 wikipediaList)
    ]
  where
    wikipediaList :: AdjList [] NodeAndWeight
    wikipediaList = AdjList $
      M.fromList [
      (1, [NodeAndWeight 2 7, NodeAndWeight 3 9, NodeAndWeight 6 14]),
      (2, [NodeAndWeight 1 7, NodeAndWeight 3 10, NodeAndWeight 4 15]),
      (3, [NodeAndWeight 1 9, NodeAndWeight 2 10, NodeAndWeight 4 11, NodeAndWeight 6 2]),
      (4, [NodeAndWeight 2 15, NodeAndWeight 3 11, NodeAndWeight 5 6]),
      (5, [NodeAndWeight 4 6, NodeAndWeight 6 9]),
      (6, [NodeAndWeight 1 14, NodeAndWeight 3 2, NodeAndWeight 5 9])
      ]
