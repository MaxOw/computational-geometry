--------------------------------------------------------------------------------
-- |
-- Module     : Geometry.SetOperations.Volume
-- Copyright  : (C) 2017 Maksymilian Owsianny
-- License    : BSD-style (see LICENSE)
-- Maintainer : Maksymilian.Owsianny@gmail.com
--
-- Set Operations of Polytopes by Boundary Filtering.
--
--------------------------------------------------------------------------------
module Geometry.SetOperations.Volume
    ( Volume (..)
    , makeVolume
    , emptyVolume
    , mergeVolumes

    , Volume2D, Volume3D
    ) where

import Protolude
import Linear (V2, V3)

import Geometry.SetOperations.Merge
import Geometry.SetOperations.Types
import Geometry.SetOperations.BSP
import Geometry.SetOperations.Facet
import Geometry.SetOperations.Clip
import Geometry.Plane.General

--------------------------------------------------------------------------------

-- | Volume, currently represented as a list of Facets and a BSP Tree.
data Volume b v n = Volume
   { volumeFacets :: [Facet b v n]
   , volumeTree   :: BSP (Plane v n)
   }

type Volume2D = Volume (FB2 V2 Double) V2 Double
type Volume3D = Volume (FB3 V3 Double) V3 Double

-- | Construct Volume from a list of Facets representing it's boundary.
makeVolume :: Clip b v n => [Facet b v n] -> Volume b v n
makeVolume fs = Volume fs (constructBSP facetPlane fs)

-- | Empty volume.
emptyVolume :: Volume b v n
emptyVolume = Volume [] Out

--------------------------------------------------------------------------------

{-# SPECIALIZE
  mergeVolumes :: SetOperation -> Volume2D -> Volume2D -> Volume2D #-}
{-# SPECIALIZE
  mergeVolumes :: SetOperation -> Volume3D -> Volume3D -> Volume3D #-}

-- | Merge two Volumes under a specified Set Operation.
mergeVolumes :: (Clip b v n, Functor v, Num n)
    => SetOperation -> Volume b v n -> Volume b v n -> Volume b v n
mergeVolumes op volumeA volumeB = case op of
    Difference          -> filterBoth isOut    isInFlip
    Intersection        -> filterBoth isIn     isIn
    Union               -> filterBoth isOut    isOut
    SymmetricDifference -> filterBoth isEither isEither
    where
    isInFlip x fs = case x of Red -> []; Green -> map flipFacet fs
    isIn     x fs = case x of Red -> []; Green -> fs
    isOut    x fs = case x of Red -> fs; Green -> []
    isEither x fs = case x of Red -> fs; Green -> map flipFacet fs

    Volume facetsA treeA = volumeA
    Volume facetsB treeB = volumeB

    filterBoth f g = makeVolume $
        filterWith f facetsA treeB <>
        filterWith g facetsB treeA

    filterWith _ [] _ = []
    filterWith f fs t = case t of
        Leaf x             -> f x fs
        Node treeL p treeR ->
            filterWith f partL treeL <>
            filterWith f partR treeR
            where (partL, partR) = splitWith (splitFacet p) fs

