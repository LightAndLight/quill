module Test.Graph (graphTests) where

import qualified Data.Graph as Graph
import qualified Data.Graph.Extra as Graph (roots)
import qualified Data.Set as Set
import Test.Hspec

graphTests :: Spec
graphTests = do
  describe "Data.Graph.Extras" $
    describe "roots" $ do
      it "single element graph - acyclic" $ do
        let (graph, fromVertex, toVertex) = Graph.graphFromEdges [("one", 1::Int, [])]
        Set.map fromVertex (Graph.roots graph fromVertex toVertex) `shouldBe` Set.fromList [("one", 1, [])]
      it "single element graph - cyclic" $ do
        let (graph, fromVertex, toVertex) = Graph.graphFromEdges [("one", 1::Int, [1])]
        Graph.roots graph fromVertex toVertex `shouldBe` mempty
      it "two element graph - acyclic 1" $ do
        let (graph, fromVertex, toVertex) = Graph.graphFromEdges [("one", 1::Int, [2]), ("two", 2, [])]
        Set.map fromVertex (Graph.roots graph fromVertex toVertex) `shouldBe` Set.fromList [("one", 1, [2])]
      it "two element graph - acyclic 2" $ do
        let (graph, fromVertex, toVertex) = Graph.graphFromEdges [("one", 1::Int, []), ("two", 2, [])]
        Set.map fromVertex (Graph.roots graph fromVertex toVertex) `shouldBe`
          Set.fromList [("one", 1, []), ("two", 2, [])]
      it "two element graph - cyclic 1" $ do
        let (graph, fromVertex, toVertex) = Graph.graphFromEdges [("one", 1::Int, [1]), ("two", 2, [])]
        Set.map fromVertex (Graph.roots graph fromVertex toVertex) `shouldBe` Set.fromList [("two", 2, [])]
      it "two element graph - cyclic 2" $ do
        let (graph, fromVertex, toVertex) = Graph.graphFromEdges [("one", 1::Int, [2]), ("two", 2, [1])]
        Graph.roots graph fromVertex toVertex `shouldBe` mempty

