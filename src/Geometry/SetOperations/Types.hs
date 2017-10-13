--------------------------------------------------------------------------------
-- |
-- Module     : Geometry.SetOperations.Types
-- Copyright  : (C) 2017 Maksymilian Owsianny
-- License    : BSD-style (see LICENSE)
-- Maintainer : Maksymilian.Owsianny@gmail.com
--
--------------------------------------------------------------------------------
module Geometry.SetOperations.Types
    ( SetOperation (..)
    ) where

--------------------------------------------------------------------------------

-- | Four basic set operations:
--
-- <<images/set-operation-examples.png>>

data SetOperation
   = Union
   | Intersection
   | Difference
   | SymmetricDifference

