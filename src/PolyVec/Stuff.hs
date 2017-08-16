{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module PolyVec.Stuff
       ( None(..)
       , Tuple(..)
       , Triple(..)
       , Quad(..)
       ) where

import GHC.Generics

import qualified Data.Foldable as F
import qualified Data.Traversable as T

import PolyVec.Class ( PolyVec(..) )

-- | a length-0 vectorizable type
data None a = None
            deriving (Eq, Ord, Generic, Generic1, Functor, F.Foldable, T.Traversable, Show)
instance PolyVec a => PolyVec (None a)


-- | a length-2 vectorizable type
data Tuple f g a = Tuple { unFst :: f a, unSnd :: g a }
                 deriving (Eq, Ord, Generic, Generic1, Functor, F.Foldable, T.Traversable, Show)
instance (PolyVec (f a), PolyVec (g a)) => PolyVec (Tuple f g a)


-- | a length-3 vectorizable type
data Triple f g h a = Triple { unFst3 :: f a, unSnd3 :: g a, unThd3 :: h a }
                    deriving (Eq, Ord, Generic, Generic1, Functor, F.Foldable, T.Traversable, Show)
instance (PolyVec (f a), PolyVec (g a), PolyVec (h a)) => PolyVec (Triple f g h a)


-- | a length-4 vectorizable type
data Quad f g h i a = Quad { unFst4 :: f a, unSnd4 :: g a, unThd4 :: h a, unFth4 :: i a }
                    deriving (Eq, Ord, Generic, Generic1, Functor, F.Foldable, T.Traversable, Show)
instance (PolyVec (f a), PolyVec (g a), PolyVec (h a), PolyVec (i a)) => PolyVec (Quad f g h i a)
