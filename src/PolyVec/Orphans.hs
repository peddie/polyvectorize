{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators #-}

module PolyVec.Orphans () where

import Control.Compose ( (:.)(..), Id(..) )
import qualified Linear
import SpatialMath ( Euler )
import SpatialMathT ( V3T, Rot )

import PolyVec.Class ( PolyVec(..) )

instance PolyVec a => PolyVec (Linear.V0 a)
instance PolyVec a => PolyVec (Linear.V1 a)
instance PolyVec a => PolyVec (Linear.V2 a)
instance PolyVec a => PolyVec (Linear.V3 a)
instance PolyVec a => PolyVec (Linear.V4 a)
instance PolyVec a => PolyVec (Linear.Quaternion a)
instance PolyVec a => PolyVec (Euler a)
instance PolyVec a => PolyVec (V3T f a)
instance PolyVec a => PolyVec (Id a)
instance PolyVec (g a) => PolyVec (Rot f1 f2 g a)
instance PolyVec (f (g a)) => PolyVec ((f :. g) a)
