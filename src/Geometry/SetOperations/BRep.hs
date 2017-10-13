{-# Language MultiParamTypeClasses #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
--------------------------------------------------------------------------------
-- |
-- Module     : Geometry.SetOperations.BRep
-- Copyright  : (C) 2017 Maksymilian Owsianny
-- License    : BSD-style (see LICENSE)
-- Maintainer : Maksymilian.Owsianny@gmail.com
--
-- Boundary representations for conversion to and from BSP/Volumes.
--
--------------------------------------------------------------------------------
module Geometry.SetOperations.BRep
    ( FromPolytopeRep (..)
    , ToPolytopeRep   (..)

    , Poly3 (..), Poly3D
    , PolyT3 (..), PolyT3D
    ) where

import Protolude
import Linear.Affine (Point)
import Linear
import qualified Data.Map as Map

import Data.EqZero

-- import qualified Data.Vector.Generic as Vector
import Data.Vector.Generic ((!))
import qualified Data.Vector as T

import Geometry.Plane.General
import Geometry.SetOperations.Facet
import Geometry.SetOperations.CrossPoint
import Geometry.SetOperations.Clip

-- | Convert from polytope to a list of Facets.
class FromPolytopeRep p b v n where
    fromPolytopeRep :: p v n -> [Facet b v n]

-- | Convert from list of Facets to a polytope boundary representation.
class ToPolytopeRep p b v n where
    toPolytopeRep :: [Facet b v n] -> p v n

--------------------------------------------------------------------------------

-- | Indexed 3-BRep as a list of convex polygons. Continent as a way to
-- introduce new base shapes into the constructive geometry context.
data Poly3 v n = Poly3 (T.Vector (Point v n)) [[Int]]
type Poly3D = Poly3 V3 Double

instance ( MakePlane v n, Eq (v n), Foldable v, Applicative v, R3 v
         , Num n, Ord n, EqZero n
         ) => FromPolytopeRep Poly3 (FB3 v n) v n where
    fromPolytopeRep = makeFacets3

{-# SPECIALIZE makeFacets3 :: Poly3D -> [Facet3D] #-}

-- I assume valid indexes for now, without checks.
-- Will need to make it safe in the future.
-- There is also assumption that each point is shared by 3 planes
-- and that each eadge is shared by 2 planes.
makeFacets3 :: (MakePlane v n, Foldable v, Applicative v, R3 v, Ord n, EqZero n)
    => (Num n, Eq (v n))
    => Poly3 v n -> [Facet (FB3 v n) v n]
makeFacets3 (Poly3 ps is) = zipWith Facet planes boundries
    where
    points = map (map (ps!)) is
    planes = map (\(a:b:c:_) -> unsafeMakePlane $ vec3 a b c) points

    mkPlaneEdge (p, es) = map (,[p]) es

    edges    = map (map mkOrdPair . edges2) is
    edgesMap = Map.fromListWith (<>) $ concatMap mkPlaneEdge $ zip planes edges

    edgePlanePairs = map (mapMaybe (flip Map.lookup edgesMap)) edges
    edgePlanes     = zipWith edgeOnly planes edgePlanePairs
    edgeOnly p es  = map (\(a:b:_) -> if p == a then b else a) es

    uniqueCrossPoints = fmap toCrossPoint ps
    crossPoints       = map (map (uniqueCrossPoints!)) is

    boundries = zipWith (\a b -> zip a b) crossPoints edgePlanes

data OrdPair a = OrdPair !a !a deriving (Show, Eq, Ord)
mkOrdPair :: Ord a => (a, a) -> OrdPair a
mkOrdPair (a, b) = if a > b then OrdPair a b else OrdPair b a

{-# INLINE edges2 #-}
edges2 :: [a] -> [(a,a)]
edges2 as = zip as (drop 1 $ cycle as)

--------------------------------------------------------------------------------

-- | Simple direct 3-BRep as a list of triangles. Useful as an output after
-- performing specified set operations of the base shapes for rendering.
newtype PolyT3 v n = PolyT3 [ [Point v n] ]

type PolyT3D = PolyT3 V3 Double

instance ToPolytopeRep PolyT3 (FB3 v n) v n where
    toPolytopeRep fs = PolyT3 (concatMap f fs)
      where
      f (Facet _ bd) = tris $ map (getPoint . fst) bd

tris :: [a] -> [[a]]
tris ps = take triNum $ concat $ zipWith mkTri pps rps
    where
    triNum = length ps - 2
    pps    = egs ps
    rps    = egs $ reverse ps
    egs xs = zip xs $ drop 1 xs
    mkTri (a,b) (n,m) = [[a, m, n], [m, a, b]]

