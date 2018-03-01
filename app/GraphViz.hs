-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  2018 Battelle Memorial Institute
-- License     :  BSD3 (see the LICENSE.txt and WARRANTY.txt files in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module Main (main) where

import qualified Data.Attoparsec.ByteString.Char8 (parseOnly)
import qualified Data.ByteString (getContents)
import           Data.Graph.Inductive.Graph (Node)
import qualified Data.Graph.Inductive.Graph (mkGraph)
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.GraphViz (GraphvizParams(clusterBy, clusterID, fmtCluster, fmtEdge, fmtNode, isDirected, isDotCluster), NodeCluster(..))
import qualified Data.GraphViz (defaultParams, graphToDot, printDotGraph)
import           Data.GraphViz.Attributes.Complete (Attribute(Color, FontColor, Label, Shape, Style), Label(StrLabel), Number(Int), Shape(BoxShape, PointShape), StyleItem(SItem), StyleName(Dashed, Invisible, Solid))
import qualified Data.GraphViz.Attributes.Colors (toColor, toWColor)
import           Data.GraphViz.Attributes.Colors.X11 (X11Color(Black, Gray))
import           Data.GraphViz.Types.Canonical (GlobalAttributes(..), GraphID(Num))
import qualified Data.List (inits)
import qualified Data.List.NonEmpty (fromList, toList)
import           Data.Map (Map)
import qualified Data.Map (elems, empty, fromList, insert, lookup, singleton, union)
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Set (empty, foldr, fromList, insert, member, toList, union)
import           Data.Text (Text)
import qualified Data.Text.Lazy.IO (hPutStrLn)
import           Data.Tuple (swap)
import qualified IDF.Parser.ByteString.Char8 (document)
import           IDF.Types (Document(documentRoot), Element(elementObjects), Name(..), Object(objectName, objectValues), Value(Reference))
import qualified System.Exit (exitFailure, exitSuccess)
import qualified System.IO (hPutStr, hPrint, stderr, stdout)
import qualified Text.PrettyPrint.Leijen.Text (Pretty(pretty), displayT, renderCompact)

main :: IO ()
main = do
  source <- Data.ByteString.getContents
  case Data.Attoparsec.ByteString.Char8.parseOnly IDF.Parser.ByteString.Char8.document source of
    Left err -> do
      System.IO.hPutStr System.IO.stderr "Error: "
      System.IO.hPrint System.IO.stderr err
      System.Exit.exitFailure
    Right document -> do
      Data.Text.Lazy.IO.hPutStrLn System.IO.stdout $ Data.GraphViz.printDotGraph $ Data.GraphViz.graphToDot getParams $ toGraph document
      System.Exit.exitSuccess

data GrNodeLabel = RootNode | NotRootNode Bool Name
  deriving (Eq, Ord, Read, Show)

data GrEdgeLabel = HasParent | References
  deriving (Eq, Ord, Read, Show)

getParams :: GraphvizParams Node GrNodeLabel GrEdgeLabel () GrNodeLabel
-- getParams :: GraphvizParams Node GrNodeLabel GrEdgeLabel Int GrNodeLabel
getParams = Data.GraphViz.defaultParams
  { isDirected = True
  -- , clusterBy = \ ~(node, x) -> case x of
  --     RootNode -> C 0 $ N (node, x)
  --     NotRootNode _ name -> C (Data.List.NonEmpty.length $ nameSegments name) $ N (node, x)
  -- , clusterID = Num . Int
  -- , isDotCluster = \ _ix -> True
  -- , fmtCluster = \ _ix ->
  --     [ GraphAttrs
  --         { attrs =
  --             [ Style
  --               [ SItem Invisible []
  --               ]
  --             ]
  --         }
  --     ]
  , fmtNode = \ ~(_node, x) -> case x of
      RootNode ->
        [ Color
            [ Data.GraphViz.Attributes.Colors.toWColor Gray
            ]
        , FontColor $ Data.GraphViz.Attributes.Colors.toColor Gray
        , Shape PointShape
        , Style
            [ SItem Dashed []
            ]
        ]
      NotRootNode False name ->
        [ Color
            [ Data.GraphViz.Attributes.Colors.toWColor Gray
            ]
        , FontColor $ Data.GraphViz.Attributes.Colors.toColor Gray
        , Label $ StrLabel $ Text.PrettyPrint.Leijen.Text.displayT $ Text.PrettyPrint.Leijen.Text.renderCompact $ Text.PrettyPrint.Leijen.Text.pretty name
        , Shape BoxShape
        , Style
            [ SItem Dashed []
            ]
        ]
      NotRootNode True name ->
        [ Color
            [ Data.GraphViz.Attributes.Colors.toWColor Black
            ]
        , FontColor $ Data.GraphViz.Attributes.Colors.toColor Black
        , Label $ StrLabel $ Text.PrettyPrint.Leijen.Text.displayT $ Text.PrettyPrint.Leijen.Text.renderCompact $ Text.PrettyPrint.Leijen.Text.pretty name
        , Shape BoxShape
        , Style
            [ SItem Solid []
            ]
        ]
  , fmtEdge = \ ~(_sourceNode, _destNode, x) -> case x of
      HasParent ->
        [ Color
            [ Data.GraphViz.Attributes.Colors.toWColor Gray
            ]
        , FontColor $ Data.GraphViz.Attributes.Colors.toColor Gray
        , Style
            [ SItem Dashed []
            ]
        ]
      References ->
        [ Color
            [ Data.GraphViz.Attributes.Colors.toWColor Black
            ]
        , FontColor $ Data.GraphViz.Attributes.Colors.toColor Black
        , Style
            [ SItem Solid []
            ]
        ]
  }

mkGraph :: (Ord k) => [k] -> [(k, k, a)] -> Gr k a
mkGraph nodes edges = Data.Graph.Inductive.Graph.mkGraph labNodes labEdges
  where
    -- labNodes :: (Ord a) => [LNode a]
    labNodes = zip (enumFromThen 0 1) nodes
    -- labNodeMap :: (Ord a) => Map a Node
    labNodeMap = Data.Map.fromList $ map swap labNodes
    -- labEdges :: (Ord a, Ord b) => [LEdge b]
    labEdges = mapMaybe (\ ~(src, dst, x) -> pure (\srcNode dstNode -> (srcNode, dstNode, x)) <*> Data.Map.lookup src labNodeMap <*> Data.Map.lookup dst labNodeMap) edges

toGraph :: Document -> Gr GrNodeLabel GrEdgeLabel
toGraph document = mkGraph (Data.Map.elems nodeMap) edges
  where
    activeNames :: Set Name
    activeNames = firstLevelNames `Data.Set.union` nthLevelNames
      where
        firstLevelNames :: Set Name
        firstLevelNames = Data.Set.fromList $ map objectName $ elementObjects $ documentRoot document
        nthLevelNames :: Set Name
        nthLevelNames = Data.Set.fromList $ concatMap (mapMaybe f . objectValues) $ elementObjects $ documentRoot document
            where
              f :: Value -> Maybe Name
              f (Reference name) = Just name
              f _ = Nothing
    inactiveNames :: Set Name
    inactiveNames = Data.Set.foldr (flip (foldr (\x -> if x `Data.Set.member` activeNames then id else Data.Set.insert x)) . mapMaybe (\xs -> if null xs then Nothing else Just $ Name { nameSegments = Data.List.NonEmpty.fromList xs }) . Data.List.inits . Data.List.NonEmpty.toList . nameSegments) Data.Set.empty activeNames
    nodeMap :: Map (Maybe Name) GrNodeLabel
    nodeMap =
      Data.Map.singleton Nothing RootNode
        `Data.Map.union`
      Data.Set.foldr (\name -> Data.Map.insert (Just name) $ NotRootNode True name) Data.Map.empty activeNames
        `Data.Map.union`
      Data.Set.foldr (\name -> Data.Map.insert (Just name) $ NotRootNode False name) Data.Map.empty inactiveNames
    hasParents :: Set (Maybe Name, Maybe Name, GrEdgeLabel)
    hasParents = Data.Set.foldr (\name -> let xss = Data.List.inits . Data.List.NonEmpty.toList . nameSegments $ name in flip (foldr (uncurry f)) $ zip xss $ tail xss) Data.Set.empty activeNames
      where
        f :: [Text] -> [Text] -> Set (Maybe Name, Maybe Name, GrEdgeLabel) -> Set (Maybe Name, Maybe Name, GrEdgeLabel)
        -- f [] _ = id
        f [] other_name = Data.Set.insert (Nothing, Just $ Name { nameSegments = Data.List.NonEmpty.fromList other_name }, HasParent)
        f name other_name = Data.Set.insert (Just $ Name { nameSegments = Data.List.NonEmpty.fromList name }, Just $ Name { nameSegments = Data.List.NonEmpty.fromList other_name }, HasParent)
    references :: Set (Maybe Name, Maybe Name, GrEdgeLabel)
    references = Data.Set.fromList $ concatMap (\object -> mapMaybe (f $ objectName object) . objectValues $ object) $ elementObjects $ documentRoot document
      where
        f :: Name -> Value -> Maybe (Maybe Name, Maybe Name, GrEdgeLabel)
        f name (Reference other_name) = Just (Just name, Just other_name, References)
        f _ _ = Nothing
    edges :: [(GrNodeLabel, GrNodeLabel, GrEdgeLabel)]
    edges = mapMaybe (\ ~(src, dst, x) -> pure (\srcNode dstNode -> (srcNode, dstNode, x)) <*> Data.Map.lookup src nodeMap <*> Data.Map.lookup dst nodeMap) $ Data.Set.toList $ hasParents `Data.Set.union` references
