{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators #-}

module PolyVec.Orphans () where

import Control.Compose ( (:.)(..), Id(..) )
import qualified Linear
import SpatialMath ( Euler )
import SpatialMathT ( V3T, Rot )

import PolyVec.Class ( Vectorize(..) )

instance Vectorize Linear.V0
instance Vectorize Linear.V1
instance Vectorize Linear.V2
instance Vectorize Linear.V3
instance Vectorize Linear.V4
instance Vectorize Linear.Quaternion
instance Vectorize Euler
instance Vectorize (V3T f)
instance Vectorize g => Vectorize (Rot f1 f2 g)
instance Vectorize Id
instance (Vectorize g, Vectorize f) => Vectorize (g :. f)
