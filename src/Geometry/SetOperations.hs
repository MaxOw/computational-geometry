{-# Language ConstraintKinds #-}
{-# OPTIONS_HADDOCK prune #-}
--------------------------------------------------------------------------------
-- |
-- Module     : Geometry.SetOperations
-- Copyright  : (C) 2017 Maksymilian Owsianny
-- License    : BSD-style (see LICENSE)
-- Maintainer : Maksymilian.Owsianny@gmail.com
--
-- Set Operations of Polytopes. You can read about implementation details of
-- this algorithm in a dedicated <MaxOw.github.io/posts/computational-geometry-set-operations-on-polytopes.html Blog Post>.
--
-- Small example:
--
-- > test :: SetOperation -> Double -> PolyT3D
-- > test op t = fromVolume $ merge op boxA boxB
-- >     where
-- >     boxA = cube
-- >     boxB = toVolume $ Poly3 (papply tr <$> ps) is
-- >     Poly3 ps is = cubePoly3
-- >     tr = translation (V3 (sin (t*0.3) * 0.3) 0.2 0.3)
-- >        <> aboutX (t*20 @@ deg)
-- >        <> aboutY (t*3  @@ deg)
--
-- Rendered:
--
-- <<images/setops3d.gif>>
--
--------------------------------------------------------------------------------
module Geometry.SetOperations (

    -- * Base Functionality
      Volume, emptyVolume
    , toVolume, fromVolume

    , SetOperation (..)
    , merge, merges

    -- * Selected Merge Operations
    , union, unions
    , intersection, intersections
    , difference, differences

    -- * Conversion from/to BReps
    , FromPolytopeRep
    , ToPolytopeRep

    , Poly3 (..)
    , PolyT3 (..)

    -- * Primitives
    , cubePoly3, cube

    -- * Specializations/Synonyms
    , toVolume3D
    , fromVolume3D
    , Volume2D, Volume3D

    , Poly3D
    , PolyT3D
    , Merge

    ) where

import Protolude

import Linear
import Linear.Affine (Point)
import qualified Linear.Affine as Point

-- import qualified Data.Vector.Generic as Vector
import qualified Data.Vector as T

import Geometry.SetOperations.Types
import Geometry.SetOperations.Volume
-- import Geometry.SetOperations.BSP
-- import Geometry.SetOperations.Facet
import Geometry.SetOperations.Clip
import Geometry.SetOperations.BRep
-- import Geometry.Plane.General

--------------------------------------------------------------------------------

-- | Convert an arbitrary polytope boundary representation into a Volume.
toVolume :: (FromPolytopeRep p b v n, Clip b v n, Functor v, Num n)
    => p v n -> Volume b v n
toVolume = makeVolume . fromPolytopeRep

-- | Recover a boundary representation of a Volume.
fromVolume :: ToPolytopeRep p b v n => Volume b v n -> p v n
fromVolume = toPolytopeRep . volumeFacets

-- | Convert a simple 3-BRep polyhedron to a Volume.
toVolume3D :: Poly3D -> Volume3D
toVolume3D = toVolume

-- | Reconstruct a triangulated 3-BRep from a Volume.
fromVolume3D :: Volume3D -> PolyT3D
fromVolume3D = fromVolume

--------------------------------------------------------------------------------

type Merge b v n = (Clip b v n, Functor v, Num n)

-- | Merge two Volumes under a specified Set Operation.
merge :: Merge b v n
      => SetOperation -> Volume b v n -> Volume b v n -> Volume b v n
merge = mergeVolumes

-- | Merges list of Volumes under a specified Set Operation. Empty list equals
-- empty set.
merges :: Merge b v n => SetOperation -> [Volume b v n] -> Volume b v n
merges _  []     = emptyVolume
merges op (v:vs) = foldl' (merge op) v vs
-- As to not leak memory on folding just a strict left fold is not enough. The
-- merge operation also needs to be strict since it operates on record with lazy
-- fields of spine lazy structures. Should I change representation to strict or
-- just deepseq it here? TODO: Fix this.

--------------------------------------------------------------------------------

-- | Union of two volumes. Convenience synonym for `merge Union`
union :: Merge b v n => Volume b v n -> Volume b v n -> Volume b v n
union = merge Union

-- | Union of list of volumes.
unions :: Merge b v n => [Volume b v n] -> Volume b v n
unions = merges Union

-- | Intersection of two volumes.
intersection :: Merge b v n => Volume b v n -> Volume b v n -> Volume b v n
intersection = merge Intersection

-- | Intersection of list of volumes.
intersections :: Merge b v n => [Volume b v n] -> Volume b v n
intersections = merges Intersection

-- | Difference between two volumes.
difference :: Merge b v n => Volume b v n -> Volume b v n -> Volume b v n
difference = merge Difference

-- | Subtract list of volumes from a given volume.
differences :: Merge b v n => Volume b v n -> [Volume b v n] -> Volume b v n
differences = foldl' (merge Difference)

--------------------------------------------------------------------------------

p3 :: a -> a -> a -> Point V3 a
p3 x y z = Point.P $ V3 x y z

-- | Cube represented as a denormalized list of polygons.
cubePoly3 :: Poly3D
cubePoly3 = Poly3 (T.fromList ps) is
    where
    ps = map (subtract 0.5) $
       [ p3 0 0 0, p3 1 0 0, p3 1 0 1, p3 0 0 1
       , p3 0 1 0, p3 1 1 0, p3 1 1 1, p3 0 1 1 ]

    is = map reverse
         [ [ 0, 1, 2, 3 ]
         , [ 1, 5, 6, 2 ]
         , [ 3, 2, 6, 7 ]
         , [ 0, 3, 7, 4 ]
         , [ 7, 6, 5, 4 ]
         , [ 0, 4, 5, 1 ]
         ]

-- | Cube volume.
cube :: Volume3D
cube = toVolume cubePoly3

