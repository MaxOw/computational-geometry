--------------------------------------------------------------------------------
-- |
-- Module     : Geometry.SetOperations.Facet
-- Copyright  : (C) 2017 Maksymilian Owsianny
-- License    : BSD-style (see LICENSE)
-- Maintainer : Maksymilian.Owsianny@gmail.com
--
--------------------------------------------------------------------------------
module Geometry.SetOperations.Facet
    ( Facet (..)
    , Facet2D, Facet3D
    , flipFacet
    , FB2, FB3

    ) where

import Protolude
import Linear (V2, V3)

import Geometry.Plane.General
import Geometry.SetOperations.CrossPoint

--------------------------------------------------------------------------------

data Facet b v n = Facet
   { facetPlane    :: Plane v n
   , facetBoundary :: b
   }

-- | Flip orientation of a facet.
flipFacet :: (Functor v, Num n) => Facet b v n -> Facet b v n
flipFacet (Facet p b) = Facet (flipPlane p) b

type FB3 v n = [(CrossPoint v n, Plane v n)]
type FB2 v n = (CrossPoint v n, CrossPoint v n)

type Facet2D = Facet (FB2 V2 Double) V2 Double
type Facet3D = Facet (FB3 V3 Double) V3 Double

