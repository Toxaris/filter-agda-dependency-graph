{-# LANGUAGE NamedFieldPuns, ParallelListComp #-}
module Main where

import Prelude (Num ((+), (-)))

import Control.Monad (return, filterM)

import Data.Array ((!))
import Data.Bool (Bool (False, True), (&&), (||), not)
import Data.Eq (Eq ((==), (/=)))
import Data.Foldable (toList)
import Data.Function ((.), ($))
import Data.Graph (graphFromEdges, path)
import Data.GraphViz (parseDotGraph, printDotGraph)
import Data.GraphViz.Attributes.Complete
  ( Attribute (Color, Label, Style, Shape)
  , Color (BrewerColor)
  , Label (StrLabel)
  , Shape (BoxShape)
  , StyleItem (SItem)
  , StyleName (Filled, Invisible)
  , WeightedColor (WC)
  )
import Data.GraphViz.Attributes.Colors.Brewer
  ( BrewerColor (BC)
  , BrewerName (Accent)
  , BrewerScheme (BScheme)
  )
import Data.GraphViz.Types.Generalised
  ( DotGraph (DotGraph), graphStatements
  , DotNode (DotNode), nodeID, nodeAttributes
  , DotEdge (DotEdge), fromNode, toNode, edgeAttributes
  , DotSubGraph (DotSG), isCluster, subGraphID, subGraphStmts
  , DotStatement (DN, DE, SG)
  , GraphID (Str)
  )
import Data.Int (Int)
import Data.List (map, null, drop, elem, notElem, sortBy, genericLength, take, (++))
import Data.Maybe (fromJust, Maybe (Just, Nothing))
import qualified Data.Map as Map (toList, fromList, fromListWith, size, lookup)
import Data.Ord (Down (Down), comparing)
import Data.Sequence as Seq (fromList)
import Data.Set as Set (fromList, member)
import Data.Text.Lazy (Text, splitOn, pack, unpack, takeWhile, break)
import qualified Data.Text.Lazy as Text (null, drop)
import Data.Text.Lazy.IO (getContents, putStrLn)
import Data.Tuple (fst, snd)

import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.FilePath (FilePath, joinPath, (</>), (<.>))
import System.IO (IO, print)

-- Deciding whether to include or exclude a file

includes :: FilePath -> FilePath -> IO Bool
includes srcpath modulePath = do
  let b0 = modulePath `notElem` ["README", "Everything"]
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

  -- the list of module prefixes, sorted by usage count
  let prefixes = take 8 $ map fst $ sortBy (comparing (Down . snd)) $ Map.toList $ Map.fromListWith (+)
        [ (prefix, 1)
        | (nodeID, label) <- modules
        , let (prefix, rest) = break ('.' ==) label
        , not (Text.null rest)
        ]

  -- the map of module prefixes to their indexes in the prefixes list
  let prefixColor = Map.fromList
        [ (prefix, color)
        | prefix <- prefixes
        | color <- [1..]
        ]

  -- the highest color number we need
  let colors = genericLength prefixes

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

  -- color encoding
  let encodeColor color colors =
        Color [WC (BrewerColor (BC (BScheme Accent colors) color)) Nothing]

  -- color-code the module prefix
  let
    adjustNode (DN (node@DotNode {nodeID, nodeAttributes})) =
      DN (node {nodeAttributes = adjustAttributes nodeAttributes})
    adjustNode x = x

    adjustAttributes attrs = [result | attr <- attrs, result <- adjustAttribute attr]

    adjustAttribute (Label (StrLabel label)) =
      let (prefix, rest) = break ('.' ==) label
      in case Map.lookup prefix prefixColor of
        Just color ->
          [ Label (StrLabel (Text.drop 1 rest))
          , encodeColor color colors
          , Style [SItem Filled []]
          ]
        Nothing ->
          [ Label (StrLabel label) ]
    adjustAttribute attr = [attr]

  -- the legend
  let legend = SG DotSG
        { isCluster = False
        , subGraphID = Nothing
        , subGraphStmts = Seq.fromList $
            [ DN DotNode
                   { nodeID = Str prefix
                   , nodeAttributes =
                       [ Label (StrLabel prefix)
                       , Shape BoxShape
                       , encodeColor color colors
                       , Style [SItem Filled []]
                       ]
                   }
            | prefix <- prefixes
            | color <- [1..]
            ] ++
            [ DE DotEdge
                   { fromNode = Str fromPrefix
                   , toNode = Str toPrefix
                   , edgeAttributes = [Style [SItem Invisible []]]
                   }
            | fromPrefix <- prefixes
            | toPrefix <- drop 1 prefixes
            ]
        }

  -- the filtered graph
  return dotgraph {graphStatements = Seq.fromList $ legend :
    [ adjustNode statement
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
