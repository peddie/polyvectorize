{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- | First pass at a polyvectorize idea.
-- This is a proof of concept implementation to think about a final design.
-- Not yet addressed:
--   PolyVec1 for kind * -> * data
--   Make it polymorphism over symbolic types.
module PolyVec.Class
       ( PolyVec(..), devectorize
       , Arrays(..), ArrayLengths(..)
       , GPolyVec(..)
       ) where

import GHC.Generics

import Data.Monoid
import Data.Int ( Int8, Int16, Int32, Int64 )
import Data.Proxy ( Proxy(..) )
import Data.Word ( Word8, Word16, Word32, Word64 )

data Arrays
  = Arrays
    { arrayInt8   :: [Int8]
    , arrayInt16  :: [Int16]
    , arrayInt32  :: [Int32]
    , arrayInt64  :: [Int64]
    , arrayWord8  :: [Word8]
    , arrayWord16 :: [Word16]
    , arrayWord32 :: [Word32]
    , arrayWord64 :: [Word64]
    , arrayFloat  :: [Float]
    , arrayDouble :: [Double]
    } deriving (Eq, Show)

instance Monoid Arrays where
  mempty =
    Arrays
    { arrayInt8   = []
    , arrayInt16  = []
    , arrayInt32  = []
    , arrayInt64  = []
    , arrayWord8  = []
    , arrayWord16 = []
    , arrayWord32 = []
    , arrayWord64 = []
    , arrayFloat  = []
    , arrayDouble = []
    }
  mappend x y =
    Arrays
    { arrayInt8   = mappend (arrayInt8   x) (arrayInt8   y)
    , arrayInt16  = mappend (arrayInt16  x) (arrayInt16  y)
    , arrayInt32  = mappend (arrayInt32  x) (arrayInt32  y)
    , arrayInt64  = mappend (arrayInt64  x) (arrayInt64  y)
    , arrayWord8  = mappend (arrayWord8  x) (arrayWord8  y)
    , arrayWord16 = mappend (arrayWord16 x) (arrayWord16 y)
    , arrayWord32 = mappend (arrayWord32 x) (arrayWord32 y)
    , arrayWord64 = mappend (arrayWord64 x) (arrayWord64 y)
    , arrayFloat  = mappend (arrayFloat  x) (arrayFloat  y)
    , arrayDouble = mappend (arrayDouble x) (arrayDouble y)
    }


data ArrayLengths
  = ArrayLengths
    { nInt8   :: Int
    , nInt16  :: Int
    , nInt32  :: Int
    , nInt64  :: Int
    , nWord8  :: Int
    , nWord16 :: Int
    , nWord32 :: Int
    , nWord64 :: Int
    , nFloat  :: Int
    , nDouble :: Int
    } deriving Show

instance Monoid ArrayLengths where
  mempty =
    ArrayLengths
    { nInt8   = 0
    , nInt16  = 0
    , nInt32  = 0
    , nInt64  = 0
    , nWord8  = 0
    , nWord16 = 0
    , nWord32 = 0
    , nWord64 = 0
    , nFloat  = 0
    , nDouble = 0
    }
  mappend x y =
    ArrayLengths
    { nInt8   = nInt8   x + nInt8   y
    , nInt16  = nInt16  x + nInt16  y
    , nInt32  = nInt32  x + nInt32  y
    , nInt64  = nInt64  x + nInt64  y
    , nWord8  = nWord8  x + nWord8  y
    , nWord16 = nWord16 x + nWord16 y
    , nWord32 = nWord32 x + nWord32 y
    , nWord64 = nWord64 x + nWord64 y
    , nFloat  = nFloat  x + nFloat  y
    , nDouble = nDouble x + nDouble y
    }

-- | calls devectorizeIncremental, makes sure there are no leftovers, and turns 'Left' into 'error'
devectorize :: PolyVec a => Arrays -> a
devectorize x = case devectorizeIncremental x of
  Left err -> error err
  Right (r, leftovers)
    | leftovers == mempty -> r
    | otherwise -> error $ "devectorize got leftover values: " ++ show leftovers

class PolyVec a where
  vectorize :: a -> Arrays
  devectorizeIncremental :: Arrays -> Either String (a, Arrays)
  vlengths :: Proxy a -> ArrayLengths

  default vectorize :: (Generic a, GPolyVec (Rep a)) => a -> Arrays
  vectorize x = gvectorize (from x)

  default devectorizeIncremental :: (Generic a, GPolyVec (Rep a)) => Arrays -> Either String (a, Arrays)
  devectorizeIncremental x = overFst to <$> gdevectorizeIncremental x

  default vlengths :: GPolyVec (Rep a) => Proxy a -> ArrayLengths
  vlengths = const $ gvlengths (Proxy :: Proxy (Rep a))


class GPolyVec a where
  gvectorize :: a p -> Arrays
  gdevectorizeIncremental :: Arrays -> Either String (a p, Arrays)
  gvlengths :: Proxy a -> ArrayLengths


-- product types
instance (GPolyVec f, GPolyVec g) => GPolyVec (f :*: g) where
  gvectorize (f :*: g) = gvectorize f <> gvectorize g
  gdevectorizeIncremental v0 = case gdevectorizeIncremental v0 of
    Left err -> Left $ "gdevectorizeIncremental (f :*: g) errored on f: " ++ err
    Right (f, v1) -> case gdevectorizeIncremental v1 of
      Left err -> Left $ "gdevectorizeIncremental (f :*: g) errored on g: " ++ err
      Right (g, v2) -> Right (f :*: g, v2)
  gvlengths = const (nf <> ng)
    where
      nf = gvlengths (Proxy :: Proxy f)
      ng = gvlengths (Proxy :: Proxy g)

overFst :: (a -> b) -> (a, c) -> (b, c)
overFst f (x, y) = (f x, y)

-- Metadata (constructor name, etc)
instance GPolyVec a => GPolyVec (M1 i c a) where
  gvectorize = gvectorize . unM1
  gdevectorizeIncremental = fmap (overFst M1) . gdevectorizeIncremental
  gvlengths = gvlengths . proxy
    where
      proxy :: Proxy (M1 i c a) -> Proxy a
      proxy = const Proxy

-- data with no fields
instance GPolyVec U1 where
  gvectorize = const mempty
  gdevectorizeIncremental v = Right (U1, v)
  gvlengths = const mempty

-- recursion
instance PolyVec a => GPolyVec (Rec0 a) where
  gvectorize = vectorize . unK1
  gdevectorizeIncremental = fmap (overFst K1) . devectorizeIncremental
  gvlengths = vlengths . proxy
    where
      proxy :: Proxy (Rec0 a) -> Proxy a
      proxy = const Proxy

-- the rest of this is the actual values
instance PolyVec Int8 where
  vectorize x = mempty {arrayInt8 = [x]}
  devectorizeIncremental v = case arrayInt8 v of
    [] -> Left "ran out of int8s"
    x0:xs -> Right (x0, v {arrayInt8 = xs})
  vlengths = const (mempty {nInt8 = 1})

instance PolyVec Int16 where
  vectorize x = mempty {arrayInt16 = [x]}
  devectorizeIncremental v = case arrayInt16 v of
    [] -> Left "ran out of int16s"
    x0:xs -> Right (x0, v {arrayInt16 = xs})
  vlengths = const (mempty {nInt16 = 1})

instance PolyVec Int32 where
  vectorize x = mempty {arrayInt32 = [x]}
  devectorizeIncremental v = case arrayInt32 v of
    [] -> Left "ran out of int32s"
    x0:xs -> Right (x0, v {arrayInt32 = xs})
  vlengths = const (mempty {nInt32 = 1})

instance PolyVec Int64 where
  vectorize x = mempty {arrayInt64 = [x]}
  devectorizeIncremental v = case arrayInt64 v of
    [] -> Left "ran out of int64s"
    x0:xs -> Right (x0, v {arrayInt64 = xs})
  vlengths = const (mempty {nInt64 = 1})

instance PolyVec Word8 where
  vectorize x = mempty {arrayWord8 = [x]}
  devectorizeIncremental v = case arrayWord8 v of
    [] -> Left "ran out of word8s"
    x0:xs -> Right (x0, v {arrayWord8 = xs})
  vlengths = const (mempty {nWord8 = 1})

instance PolyVec Word16 where
  vectorize x = mempty {arrayWord16 = [x]}
  devectorizeIncremental v = case arrayWord16 v of
    [] -> Left "ran out of word16s"
    x0:xs -> Right (x0, v {arrayWord16 = xs})
  vlengths = const (mempty {nWord16 = 1})

instance PolyVec Word32 where
  vectorize x = mempty {arrayWord32 = [x]}
  devectorizeIncremental v = case arrayWord32 v of
    [] -> Left "ran out of word32s"
    x0:xs -> Right (x0, v {arrayWord32 = xs})
  vlengths = const (mempty {nWord32 = 1})

instance PolyVec Word64 where
  vectorize x = mempty {arrayWord64 = [x]}
  devectorizeIncremental v = case arrayWord64 v of
    [] -> Left "ran out of word64s"
    x0:xs -> Right (x0, v {arrayWord64 = xs})
  vlengths = const (mempty {nWord64 = 1})

instance PolyVec Float where
  vectorize x = mempty {arrayFloat = [x]}
  devectorizeIncremental v = case arrayFloat v of
    [] -> Left "ran out of floats"
    x0:xs -> Right (x0, v {arrayFloat = xs})
  vlengths = const (mempty {nFloat = 1})

instance PolyVec Double where
  vectorize x = mempty {arrayDouble = [x]}
  devectorizeIncremental v = case arrayDouble v of
    [] -> Left "ran out of doubles"
    x0:xs -> Right (x0, v {arrayDouble = xs})
  vlengths = const (mempty {nDouble = 1})
