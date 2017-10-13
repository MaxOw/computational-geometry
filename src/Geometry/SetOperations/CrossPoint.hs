{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances     #-}
--------------------------------------------------------------------------------
-- |
-- Module     : Geometry.SetOperations.CrossPoint
-- Copyright  : (C) 2017 Maksymilian Owsianny
-- License    : BSD-style (see LICENSE)
-- Maintainer : Maksymilian.Owsianny@gmail.com
--
--------------------------------------------------------------------------------
module Geometry.SetOperations.CrossPoint
    ( Sign (..)
    , toSign

    , CrossPoint     (..)
    , MakeCrossPoint (..)
    , toCrossPoint

    ) where

import Protolude
import Linear
import Linear.Affine (Point)
import qualified Linear.Affine as Point

import Data.EqZero
import Geometry.Plane.General

--------------------------------------------------------------------------------

data Sign = M | Z | P deriving (Show, Eq)

toSign :: (EqZero n, Ord n, Num n) => n -> Sign
toSign x
    | eqZero x  = Z
    | x < 0     = M
    | otherwise = P

data CrossPoint v n = CP
   { orientation :: Plane v n -> Sign
   , getPoint    :: Point v n
   }

-- | Convert a point to CrossPoint
toCrossPoint :: (EqZero n, Foldable v, Num n, Ord n)
    => Point v n -> CrossPoint v n
toCrossPoint pt = CP orient pt
    where
    orient p = toSign . ((planeLast p) +) . sum
             $ zipWith (*) (toList $ planeVector p) (toList pt)

class MakeCrossPoint v n where
    makeCrossPoint :: v (Plane v n) -> Maybe (CrossPoint v n)

instance (Fractional n, Ord n, EqZero n) => MakeCrossPoint V2 n where
    makeCrossPoint planes
        | eqZero d2 = Nothing
        | otherwise = Just $ CP orient solved
        where
        V2     (Plane (V2 a b) c)
               (Plane (V2 d e) f) = planes
     -- orient (Plane (V2 g h) i) = toSign $ d2*(g*d0 - h*d1 + i*d2)
        orient (Plane (V2 g h) i) = toSign $ g*dd0 + h*dd1 + i

        dd0 = d2*d0
        dd1 = d2*d1

        d0 = b*f - c*e
        d1 = a*f - c*d
        d2 = a*e - b*d

        dd = 1/d2
        solved = Point.P $ V2 (dd*d0) (dd*d1)

instance (Fractional n, Ord n, EqZero n) => MakeCrossPoint V3 n where
    makeCrossPoint planes
        | eqZero d3 = Nothing
        | otherwise = Just $ CP orient solved
        where
        V3     (Plane (V3 a b c) d)
               (Plane (V3 e f g) h)
               (Plane (V3 i j k) l) = planes
        orient (Plane (V3 m n o) p) = toSign $ -d3*(m*d0 - n*d1 + o*d2 - p*d3)

        d0 = k*m1 - j*m0 + l*m2
        d1 = k*m3 - i*m0 + l*m4
        d2 = j*m3 - i*m1 + l*m5
        d3 = i*m2 - j*m4 + k*m5

        m0 = c*h - d*g
        m1 = b*h - d*f
        m2 = c*f - b*g
        m3 = a*h - d*e
        m4 = c*e - a*g
        m5 = b*e - a*f

        dd = 1/d3
        solved = Point.P $ V3 (-dd*d0) (dd*d1) (-dd*d2)

