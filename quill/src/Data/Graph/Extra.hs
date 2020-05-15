{-# language ScopedTypeVariables #-}
module Data.Graph.Extra (roots) where

import Data.Foldable (foldl')
import Data.Graph (Graph, Vertex)
import qualified Data.Graph as Graph
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)

roots ::
  forall node key.
  Graph ->
  (Vertex -> (node, key, [key])) ->
  (key -> Maybe Vertex) ->
  Set Vertex
roots graph fromVertex toVertex = Map.keysSet finalChecked
  where
    vertices :: [Vertex]
    vertices = Graph.vertices graph

    -- a mapping from vertex to 'visited'
    initialChecked :: Map Vertex Bool
    initialChecked = foldl' (\acc v -> Map.insert v False acc) mempty vertices

    markReachable :: Map Vertex Bool -> key -> Map Vertex Bool
    markReachable checked key =
      let
        v = Maybe.fromJust $ toVertex key
        (m_visited, checked') = Map.updateLookupWithKey (\_ _ -> Nothing) v checked
      in
        case m_visited of
          Nothing -> checked'
          Just visited ->
            if visited
            then checked'
            else
              let
                (_, _, nexts) = fromVertex v
              in
                foldl' markReachable checked' nexts

    -- 1. For each vertex in the graph
    -- 2. Find the vertices that are directly reachable from it
    -- 3. For each directly reachable vertex
    -- 4. Remove it and its transitively reachable vertices from root candidacy
    finalChecked :: Map Vertex Bool
    finalChecked =
      foldl'
        (\checked v ->
           let
             (_, _, nexts) = fromVertex v -- 2.
           in
             foldl'
               markReachable -- 4.
               (Map.adjust (const True) v checked)
               nexts -- 3.
        )
        initialChecked
        vertices -- 1.
