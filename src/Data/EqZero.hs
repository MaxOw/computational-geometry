{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module     : Data.EqZero
-- Copyright  : (C) 2017 Maksymilian Owsianny
-- License    : BSD-style (see LICENSE)
-- Maintainer : Maksymilian.Owsianny@gmail.com
--
--------------------------------------------------------------------------------
module Data.EqZero where

import Protolude
import Linear.Epsilon (nearZero)
import Foreign.C (CFloat, CDouble)

--------------------------------------------------------------------------------

-- | Convenient universal zero equality predicate that warps to zero within some
-- epsilon for floating point numbers.
class EqZero a where
    eqZero :: a -> Bool

{-
instance (Num a, Eq a) => EqZero Exact a where
    eqZero _ = (==0)

instance Epsilon a => EqZero NonExact a where
    eqZero _ = nearZero
-}

-- | Greater or equal to zero predicate.
geqZero :: (EqZero n, Ord n, Num n) => n -> Bool
geqZero n = eqZero n || n > 0

instance EqZero Float    where eqZero = nearZero
instance EqZero Double   where eqZero = nearZero
instance EqZero CFloat   where eqZero = nearZero
instance EqZero CDouble  where eqZero = nearZero
instance EqZero Rational where eqZero = (==0)

