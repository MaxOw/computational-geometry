{-# Language MultiParamTypeClasses #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language DefaultSignatures #-}
--------------------------------------------------------------------------------
-- |
-- Module     : Geometry.SetOperations.Clip
-- Copyright  : (C) 2017 Maksymilian Owsianny
-- License    : BSD-style (see LICENSE)
-- Maintainer : Maksymilian.Owsianny@gmail.com
--
--------------------------------------------------------------------------------
module Geometry.SetOperations.Clip
    ( Clip (..)
    , vec3
    ) where

import Data.Function (id)
import Data.List (zipWith3, unzip)
import Protolude
import Linear
import Lens.Family ((.~), over)
import Lens.Family.Stock (both)
-- import Control.Lens ((.~), over, both)

import Data.EqZero
import Geometry.Plane.General
import Geometry.SetOperations.Facet
import Geometry.SetOperations.CrossPoint

--------------------------------------------------------------------------------

class Clip b v n where
    clipFacet  :: Plane v n   -- ^ Clipping plane
               -> Facet b v n -- ^ Facet to clip
               -> Maybe (Facet b v n)

    splitFacet :: Plane v n   -- ^ Splitting plane
               -> Facet b v n -- ^ Facet to split
               -> (Maybe (Facet b v n), Maybe (Facet b v n))

    clipFacet  p f = fst $ splitFacet p f
    default splitFacet :: (Functor v, Num n)
                       => Plane v n -> Facet b v n
                       -> (Maybe (Facet b v n), Maybe (Facet b v n))
    splitFacet p f = (clipFacet p f, clipFacet (flipPlane p) f)

    {-# MINIMAL (clipFacet | splitFacet) #-}

--------------------------------------------------------------------------------

splitCoincident :: (Foldable v, Num n, Ord n, EqZero n)
    => Plane v n -> Facet b v n
    -> (Maybe (Facet b v n), Maybe (Facet b v n))
    -> (Maybe (Facet b v n), Maybe (Facet b v n))
splitCoincident h f@(Facet s _) othercase = case planesRelation h s of
    Parallel CoIncident   CoOriented -> (Just f, Nothing)
    Parallel CoIncident AntiOriented -> (Nothing, Just f)
    _ -> othercase

vec2 :: (R2 v, Applicative v) => n -> n -> v n
vec2 x y = pure x & _xy .~ (V2 x y)

instance
    ( MakeCrossPoint v n, R2 v, Applicative v
    , Foldable v, Num n, Ord n, EqZero n )
    => Clip (FB2 v n) v n where
    splitFacet h f@(Facet s (a, b)) = splitCoincident h f othercase
      where
      mc     = makeCrossPoint $ vec2 h s
      go x y = Just $ Facet s (x, y)

      othercase = table (orientation a h) (orientation b h)
      table P M = (mc >>= \c -> go a c, mc >>= \c -> go c b)
      table M P = (mc >>= \c -> go c b, mc >>= \c -> go a c)
      table P _ = (Just f, Nothing)
      table _ P = (Just f, Nothing)
      table M _ = (Nothing, Just f)
      table _ M = (Nothing, Just f)
      -- This last case is not needed and is only here for completeness.
      -- It could happen if someone wrongly created a facet with edge
      -- points not lying on the facet plane (line). In such case, that
      -- facet is simply discarded by the splitting function.
      table Z Z = (Nothing, Nothing)

--------------------------------------------------------------------------------

vec3 :: (R3 v, Applicative v) => n -> n -> n -> v n
vec3 x y z = pure x & _xyz .~ (V3 x y z)

instance
    ( MakeCrossPoint v n, R3 v, Applicative v
    , Foldable v, Num n, Ord n, EqZero n )
    => Clip (FB3 v n) v n where
    splitFacet h f@(Facet s ps) = splitCoincident h f othercase
      where
      mc v = makeCrossPoint $ vec3 s h v
      go ops@(_:_:_:_) = Just $ Facet s ops
      go _             = Nothing
      ss = map (flip orientation h . fst) ps

      othercase = over both go $ splitFast mc h ss ps

splitFast
    :: (p -> Maybe c)       -- ^ Make CrossPoint from V
    -> p                    -- ^ Clipping plane H
    -> [Sign]               -- ^ Points signs relative to H
    -> [(c, p)]             -- ^ Cross Boundry
    -> ([(c, p)], [(c, p)]) -- ^ Result
splitFast mkP h ss pvs
    | all (/= M) ss = (pvs, [])
    | all (/= P) ss = ([], pvs)
    | otherwise     = (compose outPlus, compose outMinus)
    where
    (outPlus, outMinus) = unzip $ zipWith3 table pvs ss (dropCycle 1 ss)

    table (p, v) P M = case mkP v of
        Nothing -> (mk1 (p, v), id)
        Just c  -> (mk2 (p, v) (c, h), mk1 (c, v))
    table (p, v) M P = case mkP v of
        Nothing -> (id, mk1 (p, v))
        Just c  -> (mk1 (c, v), mk2 (p, v) (c, h))

    table (p, v) Z M = (mk1 (p, v), mk1 (p, h))
    table (p, v) Z P = (mk1 (p, h), mk1 (p, v))

    table pv     P _ = (mk1 pv, id)
    table pv     M _ = (id, mk1 pv)

    table _      _ _ = (id, id) -- This case should never happen
    -- If it happens it means that it's a concave boundry.

{-# INLINE compose #-}
compose :: [([a] -> [a])] -> [a]
compose fs = foldr (.) id fs []

{-# INLINE mk1 #-}
mk1 :: a -> ([a] -> [a])
mk1 a   = (a:)

{-# INLINE mk2 #-}
mk2 :: a -> a -> ([a] -> [a])
mk2 a b = (a:) . (b:)

{-# INLINE dropCycle #-}
dropCycle :: Int -> [a] -> [a]
dropCycle n = drop n . cycle

