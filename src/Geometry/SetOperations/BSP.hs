{-# Language PatternSynonyms   #-}
{-# Language DeriveFunctor     #-}
{-# Language OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module     : Geometry.SetOperations.BSP
-- Copyright  : (C) 2017 Maksymilian Owsianny
-- License    : BSD-style (see LICENSE)
-- Maintainer : Maksymilian.Owsianny@gmail.com
--
--------------------------------------------------------------------------------
module Geometry.SetOperations.BSP
    ( BinaryTree (..)
    , LeafColor  (..)
    , swapColor

    , BSP
    , cmp
    , pattern In
    , pattern Out

    , constructBSP
    , splitWith
    , destructBinaryTree

    , prettyBSP, renderH, denormalizeBSP
    ) where

import Prelude (id)
import Protolude hiding ((<>))
import Data.Monoid ((<>))

import Lens.Family (over)
import Lens.Family.Stock (both)
-- import Control.Lens (over, both)

import Data.List (unzip)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>), dot, empty)

-- import Geometry.Plane.General
import Geometry.SetOperations.Facet
import Geometry.SetOperations.Clip

--------------------------------------------------------------------------------

-- | Binary Tree parametrized by leafs and nodes
data BinaryTree l n
   = Node (BinaryTree l n) !n (BinaryTree l n)
   | Leaf !l
   deriving (Eq, Show, Functor)

instance Bifunctor BinaryTree where
    bimap f _ (Leaf x)     = Leaf (f x)
    bimap f g (Node l n r) = Node (bimap f g l) (g n) (bimap f g r)

data LeafColor = Green | Red deriving (Eq, Show)

{-# INLINE swapColor #-}
swapColor :: LeafColor -> LeafColor
swapColor Green = Red
swapColor Red   = Green

type BSP = BinaryTree LeafColor

-- | Complementary set
cmp :: BSP a -> BSP a
cmp = first swapColor

pattern In :: BSP a
pattern In  = Leaf Green

pattern Out :: BSP a
pattern Out = Leaf Red

--------------------------------------------------------------------------------

constructBSP :: Clip b v n => (Facet b v n -> c) -> [Facet b v n] -> BSP c
constructBSP _ []                     = Out
constructBSP f (facet@(Facet s _):fs) = case splitWith (splitFacet s) fs of
    ([], rs) -> Node In                  c (constructBSP f rs)
    (ls, []) -> Node (constructBSP f ls) c Out
    (ls, rs) -> Node (constructBSP f ls) c (constructBSP f rs)
    where
    c = f facet

splitWith :: (a -> (Maybe a, Maybe a)) -> [a] -> ([a], [a])
splitWith f = over both catMaybes . unzip . map f

destructBinaryTree :: BinaryTree l n -> [n]
destructBinaryTree = flip go []
    where
    go (Node l p r) = (p:) . go l . go r
    go _            = identity

--------------------------------------------------------------------------------
-- Pretty Printing - for debugging
--------------------------------------------------------------------------------

type Context k = k -> Doc

-- | Pretty print BSP tree to stdout.
prettyBSP :: (Ord f) => BSP f -> IO ()
prettyBSP bsp = putDoc $ renderH id int bspId <+> linebreak
    where
    (bspId, _) = denormalizeBSP bsp

-- | Render BSP into a horizontal tree with a given context.
renderH :: (Doc -> Doc) -> Context k -> BSP k -> Doc
renderH _ _ In  = dullcyan "✔"
renderH _ _ Out = red      "✗"
renderH ind k (Node left pivot right) = vcat
    [ dullblue (k pivot)
    , ind $ "├ " <> renderH (ind . ("│ "<>)) k left
    , ind $ "└ " <> renderH (ind . ("  "<>)) k right
    ]

-- | Denormalize BSP with integers at nodes and IntMap of values.
denormalizeBSP :: Ord n => BSP n -> (BSP Int, IntMap n)
denormalizeBSP bsp = (fmap f bsp, fsMap)
    where
    fs    = ordNub $ destructBinaryTree bsp
    isMap = Map.fromList $ zip fs [0..]
    fsMap = IntMap.fromList $ zip [0..] fs

    f p = Map.findWithDefault (-1) p isMap

