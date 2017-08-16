{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module PolyVec.Stuff
       ( devectorize
       , (:.)(..), unO
       , None(..)
       , Id(..), unId
       , Tuple(..)
       , Triple(..)
       , Quad(..)
       , vpure
       , vapply
       , vcountUp
       , vzipWith
       , vzipWith3
       , vzipWith4
       , vdiag
       , vdiag'
       , vnames
       , vnames'
       ) where

import GHC.Generics

import Accessors ( GATip, Lookup(..), accessors, flatten, flatten' )
import Control.Compose ( (:.)(..), Id(..), unO, unId )
import Data.Aeson ( FromJSON(..), ToJSON(..) )
import Data.Binary ( Binary(..) )
import qualified Data.Vector as V
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Proxy ( Proxy(..) )
import qualified Linear

import PolyVec.Class ( Vectorize(..) )

-- | a length-0 vectorizable type
data None a = None
            deriving (Eq, Ord, Generic, Generic1, Functor, F.Foldable, T.Traversable, Show)
instance Vectorize None
instance Applicative None where
  pure = const None
  (<*>) = const (const None)
instance Linear.Additive None where
instance Linear.Metric None where
instance Binary (None a)
instance FromJSON a => FromJSON (None a)
instance ToJSON a => ToJSON (None a)


-- | a length-2 vectorizable type
data Tuple f g a = Tuple { unFst :: f a, unSnd :: g a }
                 deriving (Eq, Ord, Generic, Generic1, Functor, F.Foldable, T.Traversable, Show)
instance (Vectorize f, Vectorize g) => Vectorize (Tuple f g)
instance (Applicative f, Applicative g) => Applicative (Tuple f g) where
  pure x = Tuple (pure x) (pure x)
  Tuple fx fy <*> Tuple x y = Tuple (fx <*> x) (fy <*> y)
instance (Vectorize f, Vectorize g, Applicative f, Applicative g) => Linear.Additive (Tuple f g) where
  zero = Tuple (pure 0) (pure 0)
instance (Foldable f, Foldable g, Vectorize f, Vectorize g, Applicative f, Applicative g)
         => Linear.Metric (Tuple f g)
instance (FromJSON a, FromJSON (f a), FromJSON (g a))
         => FromJSON (Tuple f g a)
instance (ToJSON a, ToJSON (f a), ToJSON (g a))
         => ToJSON (Tuple f g a)
instance (Binary (f a), Binary (g a)) => Binary (Tuple f g a)


-- | a length-3 vectorizable type
data Triple f g h a = Triple { unFst3 :: f a, unSnd3 :: g a, unThd3 :: h a }
                    deriving (Eq, Ord, Generic, Generic1, Functor, F.Foldable, T.Traversable, Show)
instance (Vectorize f, Vectorize g, Vectorize h) => Vectorize (Triple f g h)
instance (Applicative f, Applicative g, Applicative h) => Applicative (Triple f g h) where
  pure x = Triple (pure x) (pure x) (pure x)
  Triple fx fy fz <*> Triple x y z = Triple (fx <*> x) (fy <*> y) (fz <*> z)
instance (Vectorize f, Vectorize g, Vectorize h,
          Applicative f, Applicative g, Applicative h)
         => Linear.Additive (Triple f g h) where
  zero = Triple (pure 0) (pure 0) (pure 0)
instance (Foldable f, Foldable g, Foldable h,
          Vectorize f, Vectorize g, Vectorize h,
          Applicative f, Applicative g, Applicative h)
         => Linear.Metric (Triple f g h)
instance (FromJSON a, FromJSON (f a), FromJSON (g a), FromJSON (h a))
         => FromJSON (Triple f g h a)
instance (ToJSON a, ToJSON (f a), ToJSON (g a), ToJSON (h a))
         => ToJSON (Triple f g h a)
instance (Binary (f a), Binary (g a), Binary (h a)) => Binary (Triple f g h a)


-- | a length-4 vectorizable type
data Quad f g h i a = Quad { unFst4 :: f a, unSnd4 :: g a, unThd4 :: h a, unFth4 :: i a }
                    deriving (Eq, Ord, Generic, Generic1, Functor, F.Foldable, T.Traversable, Show)
instance (Vectorize f, Vectorize g, Vectorize h, Vectorize i) => Vectorize (Quad f g h i)
instance (Applicative f, Applicative g, Applicative h, Applicative i) => Applicative (Quad f g h i) where
  pure x = Quad (pure x) (pure x) (pure x) (pure x)
  Quad fx fy fz fw <*> Quad x y z w = Quad (fx <*> x) (fy <*> y) (fz <*> z) (fw <*> w)
instance (Vectorize f, Vectorize g, Vectorize h, Vectorize i,
          Applicative f, Applicative g, Applicative h, Applicative i)
         => Linear.Additive (Quad f g h i) where
  zero = Quad (pure 0) (pure 0) (pure 0) (pure 0)
instance (FromJSON a, FromJSON (f a), FromJSON (g a), FromJSON (h a), FromJSON (i a))
         => FromJSON (Quad f g h i a)
instance (ToJSON a, ToJSON (f a), ToJSON (g a), ToJSON (h a), ToJSON (i a))
         => ToJSON (Quad f g h i a)
instance (Binary (f a), Binary (g a), Binary (h a), Binary (i a)) => Binary (Quad f g h i a)

instance Lookup (None a)
instance (Lookup (f a), Lookup (g a)) => Lookup (Tuple f g a)
instance (Lookup (f a), Lookup (g a), Lookup (h a)) => Lookup (Triple f g h a)
instance (Lookup (f a), Lookup (g a), Lookup (h a), Lookup (i a)) => Lookup (Quad f g h i a)


-- | partial version of 'devectorize\'' which throws an error
-- if the vector length doesn' match the type length
devectorize :: Vectorize f => V.Vector a -> f a
devectorize x = case devectorize' x of
  Right y -> y
  Left msg -> error msg

-- | '(<*>)' in terms of 'Vectorize'
vapply :: Vectorize f => f (a -> b) -> f a -> f b
vapply f x = devectorize (V.zipWith id (vectorize f) (vectorize x))

-- | 'pure' in terms of 'Vectorize'
vpure :: forall f a . Vectorize f => a -> f a
vpure x = case devectorize' (V.replicate n x) of
  Left err -> error $ "'impossible' error in vpure: " ++ err
  Right r -> r
  where
    n = vlength (Proxy :: Proxy f)

vzipWith :: Vectorize f => (a -> b -> c) -> f a -> f b -> f c
vzipWith f x y = devectorize $ V.zipWith f (vectorize x) (vectorize y)

vzipWith3 :: Vectorize f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
vzipWith3 f x y z = devectorize $ V.zipWith3 f (vectorize x) (vectorize y) (vectorize z)

vzipWith4 :: Vectorize f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
vzipWith4 f x y z w =
  devectorize $ V.zipWith4 f (vectorize x) (vectorize y) (vectorize z) (vectorize w)

-- | Make a diagonal "matrix" from a "vector".
-- Off-diagonal elements will be 0, thus the Num constraint.
vdiag :: forall f a . (Vectorize f, Num a) => f a -> f (f a)
vdiag = flip vdiag' 0

-- | Make a diagonal "matrix" from a "vector" with a given off-diagonal value.
vdiag' :: forall f a . Vectorize f => f a -> a -> f (f a)
vdiag' v0 offDiag =
  devectorize $ V.generate n (\k -> devectorize (V.generate n (\j -> gen j k)))
  where
    v = vectorize v0
    n = vlength (Proxy :: Proxy f)
    gen j k
      | j /= k = offDiag
      | otherwise = v V.! k


-- | fill a vectorizable thing with its field names
vnames :: forall f . (Vectorize f, Lookup (f ())) => f String
vnames = case mr of
  Left msg -> error $ "vnames devectorize error: " ++ msg
  Right r -> r
  where
    mr = devectorize' $ V.fromList $
         fmap fst (flatten accessors :: [(String, GATip (f ()))])

-- | fill a vectorizable thing with its field name heirarchy
vnames' :: forall f . (Vectorize f, Lookup (f ())) => f [String]
vnames' = case mr of
  Left msg -> error $ "vnames' devectorize error: " ++ msg
  Right r -> fmap (map (maybe "()" id)) r
  where
    mr = devectorize' $ V.fromList $
         fmap fst (flatten' accessors :: [([Maybe String], GATip (f ()))])

vcountUp :: forall f . Vectorize f => f Int
vcountUp = devectorize (V.fromList (take n [0..]))
  where
    n = vlength (Proxy :: Proxy f)
