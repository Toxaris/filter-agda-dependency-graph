{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Prelude (Num ((-)))

import Control.Monad (return, filterM)

import Data.Array ((!))
import Data.Bool (Bool (False, True), (&&), (||))
import Data.Eq (Eq ((==), (/=)))
import Data.Foldable (toList)
import Data.Function ((.))
import Data.Graph (graphFromEdges, path)
import Data.GraphViz (parseDotGraph, printDotGraph)
import Data.GraphViz.Attributes.Complete
  ( Attribute (Label)
  , Label (StrLabel)
  )
import Data.GraphViz.Types.Generalised
  ( DotGraph (DotGraph), graphStatements
  , DotNode (DotNode), nodeID, nodeAttributes
  , DotEdge (DotEdge), fromNode, toNode
  , DotStatement (DN, DE)
  , GraphID
  )
import Data.List (map, null, drop, elem)
import Data.Maybe (fromJust)
import Data.Map as Map (fromList, size, lookup)
import Data.Sequence as Seq (fromList)
import Data.Set as Set (fromList, member)
import Data.Text.Lazy (Text, splitOn, pack, unpack)
import Data.Text.Lazy.IO (getContents, putStrLn)

import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.FilePath (FilePath, joinPath, (</>), (<.>))
import System.IO (IO, print)

-- Deciding whether to include or exclude a file

includes :: FilePath -> FilePath -> IO Bool
includes srcpath modulePath = do
  let b0 = modulePath /= "README"
  b1 <- doesFileExist (srcpath </> modulePath <.> "agda")
  b2 <- doesFileExist (srcpath </> modulePath <.> "lagda")
  return (b0 && (b1 || b2))

-- filtering the graph

moduleToFilePath :: Text -> FilePath
moduleToFilePath = joinPath . map unpack . splitOn (pack ".")

processGraph :: FilePath -> DotGraph GraphID -> IO (DotGraph GraphID)
processGraph srcpath dotgraph@DotGraph {graphStatements} = do
  -- all module nodes in the graph
  let modules =
        [ (nodeID, label)
        | DN DotNode {nodeID, nodeAttributes} <- toList graphStatements
        , Label (StrLabel label) <- nodeAttributes
        ]

  -- filter out modules we don't want to keep
  let decide (_, name) = do
        let modulePath = moduleToFilePath name
        includes srcpath modulePath
  modules <- filterM decide modules

  -- the set of module ids we want to keep, with indexes
  let keep = Set.fromList [nodeID | (nodeID, label) <- modules]

  -- the graph of import edges between modules we want to keep
  let (graph, fromVertex, toVertex) = graphFromEdges
        [ (from, from, to)
        | from <- toList keep
        , let to =
                [ toNode
                | DE DotEdge {fromNode, toNode} <- toList graphStatements
                , fromNode == from
                , toNode `member` keep
                ]
        ]

  -- the filtered graph
  return dotgraph {graphStatements = Seq.fromList
    [ statement
    | statement <- toList graphStatements
    , case statement of
        DN DotNode {nodeID} -> nodeID `member` keep
        DE DotEdge {fromNode, toNode} ->
          (fromNode `member` keep) &&
          (toNode `member` keep) &&
          null [ child
               | let source = fromJust (toVertex fromNode)
               , let target = fromJust (toVertex toNode)
               , child <- graph ! source
               , grandchild <- graph ! child
               , path graph grandchild target
               ]
        _ -> True
    ]}

main = do
  text <- getContents
  let input = parseDotGraph text
  output <- processGraph "." input
  putStrLn (printDotGraph output)
