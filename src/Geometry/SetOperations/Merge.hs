{-# Language MultiParamTypeClasses       #-}
{-# Language TypeSynonymInstances        #-}
{-# Language FlexibleInstances           #-}
--------------------------------------------------------------------------------
-- |
-- Module     : Geometry.SetOperations.Merge
-- Copyright  : (C) 2017 Maksymilian Owsianny
-- License    : BSD-style (see LICENSE)
-- Maintainer : Maksymilian.Owsianny@gmail.com
--
-- Set Operations of Polytopes by BSP Merging.
--
--------------------------------------------------------------------------------
module Geometry.SetOperations.Merge
    ( BSP
    , BSP3D, BSP2D

    , Universe (..)
    , universePlanes, universeBox
    , splitRegion

    , mergeBSPs
    , trim

    , makeBSP
    , toBoundary
    ) where

import Protolude
import Prelude (id)

import Lens.Family (over)
import Lens.Family.Stock (both, _2)
-- import Control.Lens (over, both, _2)

import Data.Maybe (fromMaybe, fromJust)
import Linear

import Geometry.SetOperations.Types
import Geometry.SetOperations.BSP
import Geometry.SetOperations.Facet
import Geometry.SetOperations.CrossPoint
import Geometry.SetOperations.Clip
import Geometry.Plane.General
import Data.EqZero

type BSP2D = BSP Facet2D
type BSP3D = BSP Facet3D

--------------------------------------------------------------------------------

-- Arbitrary selected as sufficient by independent comity (not really).
universeSize :: Num n => n
universeSize = 500

clipPlanes :: Clip b v n => Facet b v n -> [Plane v n] -> Facet b v n
clipPlanes = foldr (\p f -> fromMaybe f $ clipFacet p f)

class Clip b v n => Universe b v n where
    -- | Turn plane into a Facet by clipping it by the universe box.
    makeFacet :: Plane v n -> Facet b v n

instance (Ord n, Fractional n, EqZero n) => Universe (FB2 V2 n) V2 n where
    makeFacet p = clipPlanes baseFacet ps
        where
        baseFacet = Facet p (a, b)
        Just a = makeCrossPoint (V2 p pa)
        Just b = makeCrossPoint (V2 p pb)
        (pa:pb:ps) = filter (not . isParallel p) universePlanes

instance (Ord n, Fractional n, EqZero n) => Universe (FB3 V3 n) V3 n where
    makeFacet p = Facet p es
        where
        ps = filter (not . isParallel p) universePlanes
        es = zipWith mkBd ps $ drop 1 $ cycle ps
        mkBd a b = (fromJust . makeCrossPoint $ V3 p a b, b)

-- | Planes bounding the UniverseBox.
universePlanes :: (Applicative v, Traversable v, Num n) => [Plane v n]
universePlanes = positive ++ negative
    where
    toPlane v    = Plane v universeSize
    positive     = map toPlane (basisFor $ pure 0)
    negative     = map flipPlane positive

-- | List of facets bounding the Universe.
universeBox :: (Universe b v n, Applicative v, Traversable v, Num n)
    => [Facet b v n]
universeBox = map makeFacet universePlanes

-- | Split a region within a Universe bounded by a list of Facets.
splitRegion :: (Universe b v n, Functor v, Num n)
    => Plane v n -> [Facet b v n] -> ([Facet b v n], [Facet b v n])
splitRegion h fs = (flipFacet lid : plusC, lid : minusC)
    where
    (plusC, minusC) = splitWith (splitFacet h) fs
    lid = clipPlanes (makeFacet h) (map facetPlane fs)

{-
type Merge b v n =
    (Universe b v n, Applicative v, Traversable v, Num n, Ord n, EqZero n)
-}

-- | Perform a given SetOperation of two BSPs by merging
mergeBSPs
    :: (Universe b v n, Applicative v, Traversable v, Num n, Ord n, EqZero n)
    => SetOperation
    -> BSP (Facet b v n)
    -> BSP (Facet b v n)
    -> BSP (Facet b v n)
mergeBSPs op (Node treeL p treeR) nodeR@(Node _ f _) =
    collapse $ Node mTreeL p mTreeR
    where
    ff = facetPlane f
    pp = facetPlane p
    regions = splitRegion ff universeBox
    (partL, partR) = partitionBSP regions pp nodeR
    mTreeL = mergeBSPs op treeL partL
    mTreeR = mergeBSPs op treeR partR
mergeBSPs op s1 s2 = setOperation op s1 s2

partitionBSP
    :: (Universe b v n, Functor v, Foldable v, Num n, Ord n, EqZero n)
    => ([Facet b v n], [Facet b v n])
    -> Plane v n
    -> BSP (Facet b v n)
    -> (BSP (Facet b v n), BSP (Facet b v n))
partitionBSP _       _ (Leaf c)             = (Leaf c, Leaf c)
partitionBSP regions p (Node treeP f treeM) = case planesRelation p ff of
    Parallel CoIncident   CoOriented -> (treeP, treeM)
    Parallel CoIncident AntiOriented -> (treeM, treeP)
    othercase -> if
      | null regionPR -> (Node treeP f treeML, treeMR)
      | null regionMR -> (Node treePL f treeM, treePR)
      | null regionPL -> (treeML, Node treeP f treeMR)
      | null regionML -> (treePL, Node treePR f treeM)

      | otherwise     -> (Node treePL f treeML, Node treePR f treeMR)
    where
    ff = facetPlane f
    (treePL, treePR) = partitionBSP (regionPL, regionPR) p treeP
    (treeML, treeMR) = partitionBSP (regionML, regionMR) p treeM

    (regionP , regionM ) = regions
    (regionPL, regionPR) = splitRegion p regionP
    (regionML, regionMR) = splitRegion p regionM

setOperation :: SetOperation -> BSP a -> BSP a -> BSP a

setOperation    Union                  In   set = In
setOperation    Union                  Out  set = set
setOperation    Union                  set  In  = In
setOperation    Union                  set  Out = set

setOperation    Intersection           In   set = set
setOperation    Intersection           Out  set = Out
setOperation    Intersection           set  In  = set
setOperation    Intersection           set  Out = Out

setOperation    Difference             In   set = cmp set
setOperation    Difference             Out  set = Out
setOperation    Difference             set  In  = Out
setOperation    Difference             set  Out = set

setOperation    SymmetricDifference    In   set = cmp set
setOperation    SymmetricDifference    Out  set = set
setOperation    SymmetricDifference    set  In  = cmp set
setOperation    SymmetricDifference    set  Out = set

collapse :: BSP n -> BSP n
collapse (Node In  _ In ) = In
collapse (Node Out _ Out) = Out
collapse other            = other

isBoundary :: Clip b v n => BSP (Facet b v n) -> Facet b v n -> Bool
isBoundary In  _ = True
isBoundary Out _ = False
isBoundary (Node l s r) f = lcnd || rcnd
    where
    (lh, rh) = splitFacet (facetPlane s) f
    lcnd = fromMaybe False (isBoundary l <$> lh)
    rcnd = fromMaybe False (isBoundary r <$> rh)

-- | Optimize a resulting BSP after merging by removing superficial splitting
-- planes.
trim :: Clip b v n => BSP (Facet b v n) -> BSP (Facet b v n)
trim (Node Out f r)
    | isBoundary r f = Node Out f (trim r)
    | otherwise     = trim r
trim (Node l f Out)
    | isBoundary l f = Node (trim l) f Out
    | otherwise     = trim l
trim other = other

--------------------------------------------------------------------------------

-- | Make a BSP from a list of bounding facets.
makeBSP :: Clip b v n => [Facet b v n] -> BSP (Facet b v n)
makeBSP = constructBSP id

-- | Reconstruct boundary facets from the BSP.
toBoundary :: (Clip b v n, Functor v, Num n)
    => BSP (Facet b v n) -> [Facet b v n]
toBoundary bsp
    = removeColors
    . map (over _2 flipFacet)
    . applyColors
    $ destructBinaryTree bsp
    where
    applyColors xs = go xs bsp []
        where
        go [] _   = id
        go fs In  = foldr (\f cs -> ((True , f):) . cs) id fs
        go fs Out = foldr (\f cs -> ((False, f):) . cs) id fs
        go fs (Node l s r) = go ls l . go rs r
            where
            sp = facetPlane s
            (ls, rs) = splitWith (splitFacet sp) fs

    removeColors xs = go xs bsp []
        where
        go [] _   = id
        go fs In  = foldr (\(a,b) cs -> if not a then (b:) . cs else cs) id fs
        go fs Out = foldr (\(a,b) cs -> if     a then (b:) . cs else cs) id fs
        go fs (Node l s r) = go ls l . go rs r
            where
            (ls, rs) = splitWith coloredSplit fs
            sp = facetPlane s
            coloredSplit (b, f) = over both (fmap (b,)) $ splitFacet sp f


