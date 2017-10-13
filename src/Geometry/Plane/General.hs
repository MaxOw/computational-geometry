{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module     : Geometry.Plane.General
-- Copyright  : (C) 2017 Maksymilian Owsianny
-- License    : BSD-style (see LICENSE)
-- Maintainer : Maksymilian.Owsianny@gmail.com
--
-- General representation of a plane. Plane in the General Form is Hession
-- Normal Form scaled by an arbitrary non-zero scalar.
--
--------------------------------------------------------------------------------
module Geometry.Plane.General
    ( Plane (..)
    , Plane2, Plane3
    , Plane2D, Plane3D

    , MakePlane (..)
    , unsafeMakePlane
    , flipPlane

    , collinear
 -- , coincidence, coorientation

    , PlanesRelation (..), Incidence (..), Orientation (..)
    , planesRelation
    , isParallel

    ) where

import Protolude hiding (zipWith, zero)
import Data.Maybe (fromJust)
import qualified Data.List as List
import Linear
-- import Linear.Solve
import Linear.Affine (Point, (.-.))
import qualified Linear.Affine as Point
import Data.EqZero

-- | Internally Plane is represented as a pair (sN, sO) where N is a normal
-- vector of a plane O is the distance of that plane from the origin and s is an
-- arbitrary non-zero scalar.
data Plane v n = Plane
   { planeVector :: !(v n)
   , planeLast   :: !n
   } deriving (Eq, Ord, Show)

type Plane2 = Plane V2
type Plane3 = Plane V3

type Plane2D = Plane V2 Double
type Plane3D = Plane V3 Double

instance (NFData (v n), NFData n) => NFData (Plane v n) where
    rnf (Plane vs l) = rnf vs `seq` rnf l

-- | Flip plane orientation.
flipPlane :: (Functor v, Num n) => Plane v n -> Plane v n
flipPlane (Plane v n) = Plane (fmap negate v) (negate n)

class MakePlane v n where
    -- | Make plane from vector of points. Returns Nothing if vectors between
    -- points are linearly dependent
    makePlane :: v (Point v n) -> Maybe (Plane v n)

instance (Num n, Eq n) => MakePlane V3 n where
    makePlane (V3 p1 p2 p3)
        | n == zero = Nothing
        | otherwise = Just $ Plane n d
        where
        n = cross (p2 .-. p1) (p3 .-. p1)
        d = negate $ dot n $ unPoint p1

-- | Assumes that points form a valid plane (i.e. vectors between all points are
-- linearly independent).
unsafeMakePlane :: MakePlane v n => v (Point v n) -> Plane v n
unsafeMakePlane = fromJust . makePlane

{-
makePlane :: (Applicative v, Solve v n, Num n)
    => v (Point v n) -> Maybe (Plane v n)
-- makePlane ps = Plane <$> solve ups (pure 1) <*> pure 1
makePlane ps = uncurry Plane <$> solve ups (pure 1)
    where
    ups = fmap unPoint ps

-- | Assumes that points form a valid plane (i.e. vectors between all points are
-- linearly independent).
unsafeMakePlane :: (Applicative v, Solve v n, Num n)
    => v (Point v n) -> Plane v n
-- unsafeMakePlane ps = Plane (fromJust $ solve ups (pure 1)) 1
-- unsafeMakePlane ps = Plane v d
unsafeMakePlane ps = case solve ups (pure 1) of
    Just (v, d) -> Plane v d
    Nothing     -> error "Bla" -- . toS $ List.unlines $ map show ps
    where
    -- Just (v, d) = solve ups (pure 1)
    ups = fmap unPoint ps
-}

-- | Convert point to a vector.
unPoint :: Point v n -> v n
unPoint (Point.P x) = x

--------------------------------------------------------------------------------

-- | Test whether two vectors are collinear.
collinear :: (Foldable v, Num n, EqZero n) => v n -> v n -> Bool
collinear v w = all f $ combinations 2 $ zipWith (,) v w
    where
    f [(a, b), (c, d)] = eqZero $ a*d - b*c
    f _                = False -- To silence exhaustiveness checker

-- | All n-combinations of a given list.
combinations :: Int -> [a] -> [[a]]
combinations k is
    | k <= 0    = [ [] ]
    | otherwise = [ x:r | x:xs <- tails is, r <- combinations (k-1) xs ]

-- | Zip two `Foldable` structures to a list with a given function.
zipWith :: Foldable f => (a -> b -> c) -> f a -> f b -> [c]
zipWith f a b = List.zipWith f (toList a) (toList b)

-- | Test co-incidence of two planes assuming collinearity.
coincidence :: (Foldable v, Num n, EqZero n) => Plane v n -> Plane v n -> Bool
coincidence (Plane v1 d1) (Plane v2 d2) = all f $ zipWith (,) v1 v2
    where
    f (x1, x2) = eqZero $ x1*d2 - x2*d1

-- | Test co-orientation of two assuming collinearity.
coorientation :: (Foldable v, Num n, Ord n, EqZero n)
    => Plane v n -> Plane v n -> Bool
coorientation (Plane v1 d1) (Plane v2 d2)
    = all geqZero $ d1*d2 : zipWith (*) v1 v2

--------------------------------------------------------------------------------

data PlanesRelation = Parallel Incidence Orientation | Crossing deriving Show
data Incidence      = CoIncident |  NonIncident                 deriving Show
data Orientation    = CoOriented | AntiOriented                 deriving Show

-- | Relate two planes on Parallelism, Incidence and Orientation.
planesRelation :: (Foldable v, Num n, Ord n, EqZero n)
    => Plane v n -> Plane v n -> PlanesRelation
planesRelation p1@(Plane v1 _) p2@(Plane v2 _)
    | collinear v1 v2 = Parallel incidence orientation
    | otherwise       = Crossing
    where
    incidence   = bool  NonIncident CoIncident $ coincidence   p1 p2
    orientation = bool AntiOriented CoOriented $ coorientation p1 p2

isParallel :: (Foldable v, Num n, Ord n, EqZero n)
    => Plane v n -> Plane v n -> Bool
isParallel a b = case planesRelation a b of
    Parallel _ _ -> True
    Crossing     -> False

