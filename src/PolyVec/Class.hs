{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Class for turning data into arrays of consituent types.
module PolyVec.Class
       ( Arrays(..), ArrayLengths(..)
       , PolyVec(..)
       , devectorize, devectorize'
       ) where

import Generics.SOP

import Data.Vector ( Vector )
import qualified Data.Vector as V
import Data.Monoid ( Monoid(..) )
import Data.Int ( Int8, Int16, Int32, Int64 )
import Data.Proxy ( Proxy(..) )
import Data.Word ( Word8, Word16, Word32, Word64 )

data Arrays f
  = Arrays
    { arrayBool   :: Vector (f Bool)
    , arrayInt8   :: Vector (f Int8)
    , arrayInt16  :: Vector (f Int16)
    , arrayInt32  :: Vector (f Int32)
    , arrayInt64  :: Vector (f Int64)
    , arrayWord8  :: Vector (f Word8)
    , arrayWord16 :: Vector (f Word16)
    , arrayWord32 :: Vector (f Word32)
    , arrayWord64 :: Vector (f Word64)
    , arrayFloat  :: Vector (f Float)
    , arrayDouble :: Vector (f Double)
    }
instance Monoid (Arrays f) where
  mempty =
    Arrays
    { arrayBool   = V.empty
    , arrayInt8   = V.empty
    , arrayInt16  = V.empty
    , arrayInt32  = V.empty
    , arrayInt64  = V.empty
    , arrayWord8  = V.empty
    , arrayWord16 = V.empty
    , arrayWord32 = V.empty
    , arrayWord64 = V.empty
    , arrayFloat  = V.empty
    , arrayDouble = V.empty
    }
  mappend x y =
    Arrays
    { arrayBool   = arrayBool   x V.++ arrayBool   y
    , arrayInt8   = arrayInt8   x V.++ arrayInt8   y
    , arrayInt16  = arrayInt16  x V.++ arrayInt16  y
    , arrayInt32  = arrayInt32  x V.++ arrayInt32  y
    , arrayInt64  = arrayInt64  x V.++ arrayInt64  y
    , arrayWord8  = arrayWord8  x V.++ arrayWord8  y
    , arrayWord16 = arrayWord16 x V.++ arrayWord16 y
    , arrayWord32 = arrayWord32 x V.++ arrayWord32 y
    , arrayWord64 = arrayWord64 x V.++ arrayWord64 y
    , arrayFloat  = arrayFloat  x V.++ arrayFloat  y
    , arrayDouble = arrayDouble x V.++ arrayDouble y
    }

-- | useful container for counting lengths of Arrays
data ArrayLengths
  = ArrayLengths
    { nBool   :: !Word64
    , nInt8   :: !Word64
    , nInt16  :: !Word64
    , nInt32  :: !Word64
    , nInt64  :: !Word64
    , nWord8  :: !Word64
    , nWord16 :: !Word64
    , nWord32 :: !Word64
    , nWord64 :: !Word64
    , nFloat  :: !Word64
    , nDouble :: !Word64
    } deriving Show
instance Monoid ArrayLengths where
  mempty =
    ArrayLengths
    { nBool   = 0
    , nInt8   = 0
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
    { nBool   = nBool   x + nBool   y
    , nInt8   = nInt8   x + nInt8   y
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

class PolyVec f a where
  vectorize :: a -> Arrays f
  devectorizeIncremental :: Arrays f -> Either String (a, Arrays f)
  vlengths :: Proxy f -> Proxy a -> ArrayLengths

  default vectorize :: (Generic a, All2 (PolyVec f) (Code a)) => a -> Arrays f
  vectorize = gvectorize

  default devectorizeIncremental :: ( Generic a
                                    , Code a ~ '[xs]
                                    , All (PolyVec f) xs
                                    )
                                 => Arrays f -> Either String (a, Arrays f)
  devectorizeIncremental = gdevectorizeIncremental

  default vlengths :: ( Generic a
                      , Code a ~ '[xs]
                      , All (PolyVec f) xs
                      )
                   => Proxy f -> Proxy a -> ArrayLengths
  vlengths = const . const $ gvlengths (Proxy :: Proxy f) (Proxy :: Proxy a)

gvectorize :: (Generic a, All2 (PolyVec f) (Code a)) => a -> Arrays f
gvectorize x = gvectorizeS (from x)

gvectorizeS :: All2 (PolyVec f) xss => SOP I xss -> Arrays f
gvectorizeS (SOP (Z xs))  = gvectorizeP xs
gvectorizeS (SOP (S xss)) = gvectorizeS (SOP xss)

gvectorizeP :: (All (PolyVec f) xs) => NP I xs -> Arrays f
gvectorizeP Nil = mempty
gvectorizeP (I x :* xs) = vectorize x `mappend` gvectorizeP xs


gdevectorizeIncremental :: forall f a xs .
                           ( Generic a
                           , Code a ~ '[xs]
                           , All (PolyVec f) xs
                           )
                        => Arrays f -> Either String (a, Arrays f)
gdevectorizeIncremental vs0 = case gdevectorizeIncrementalP vs0 of
  Left err -> Left err
  Right (x, vs1) -> Right (to (SOP (Z x)), vs1)


gdevectorizeIncrementalP :: forall f xs .
                            (All (PolyVec f) xs)
                         => Arrays f -> Either String (NP I xs, Arrays f)
gdevectorizeIncrementalP vs0 = case (sList :: SList xs) of
  SNil -> Right (Nil, vs0)
  r@SCons -> f r
    where
      f :: forall x xs1
           . ( PolyVec f x
             , All (PolyVec f) xs1
             )
        => SList (x : xs1) -> Either String (NP I (x : xs1), Arrays f)
      f _ = case devectorizeIncremental vs0 of
        Left err -> Left err
        Right (x, vs1) -> case gdevectorizeIncrementalP vs1 of
          Left err -> Left err
          Right (xs, vs2) -> Right (I x :* xs, vs2)


gvlengths :: forall a xs f .
             ( Generic a
             , Code a ~ '[xs]
             , All (PolyVec f) xs
             )
          => Proxy f -> Proxy a -> ArrayLengths
gvlengths =
  const . const $
  gvlengthsP (Proxy :: Proxy f) (Proxy :: Proxy (NP I xs))

gvlengthsP :: forall f xs . (All (PolyVec f) xs) => Proxy f -> Proxy (NP I xs) -> ArrayLengths
gvlengthsP = const . const $ case (sList :: SList xs) of
  SNil -> mempty
  r@SCons -> f r
    where
      f :: forall x xs1 .
           ( PolyVec f x
           , All (PolyVec f) xs1
           )
        => SList (x : xs1) -> ArrayLengths
      f = const $
        vlengths (Proxy :: Proxy f) (Proxy :: Proxy x)
        `mappend`
        gvlengthsP (Proxy :: Proxy f) (Proxy :: Proxy (NP I xs1))


-- | partial version of 'devectorize\''
devectorize :: PolyVec f a => Arrays f -> a
devectorize vs = case devectorize' vs of
  Right r -> r
  Left err -> error err

devectorize' :: PolyVec f a => Arrays f -> Either String a
devectorize' vs = case devectorizeIncremental vs of
  Left err -> Left err
  Right (r, leftovers)
    | any (/= 0)
      [ V.length $ arrayBool   leftovers
      , V.length $ arrayInt8   leftovers
      , V.length $ arrayInt16  leftovers
      , V.length $ arrayInt32  leftovers
      , V.length $ arrayInt64  leftovers
      , V.length $ arrayWord8  leftovers
      , V.length $ arrayWord16 leftovers
      , V.length $ arrayWord32 leftovers
      , V.length $ arrayWord64 leftovers
      , V.length $ arrayFloat  leftovers
      , V.length $ arrayDouble leftovers
      ] -> Left "deserialise got leftovers"
    | otherwise -> Right r


-- the rest of this is the actual values
instance PolyVec f (f Bool) where
  vectorize x = mempty {arrayBool = V.singleton x}
  devectorizeIncremental v
    | V.null bools = Left "ran out of bools"
    | otherwise = Right (V.head bools, v {arrayBool = V.tail bools})
    where
      bools = arrayBool v
  vlengths = const . const $ mempty {nBool = 1}

instance PolyVec f (f Int8) where
  vectorize x = mempty {arrayInt8 = V.singleton x}
  devectorizeIncremental v
    | V.null int8s = Left "ran out of int8s"
    | otherwise = Right (V.head int8s, v {arrayInt8 = V.tail int8s})
    where
      int8s = arrayInt8 v
  vlengths = const . const $ mempty {nInt8 = 1}

instance PolyVec f (f Int16) where
  vectorize x = mempty {arrayInt16 = V.singleton x}
  devectorizeIncremental v
    | V.null int16s = Left "ran out of int16s"
    | otherwise = Right (V.head int16s, v {arrayInt16 = V.tail int16s})
    where
      int16s = arrayInt16 v
  vlengths = const . const $ mempty {nInt16 = 1}

instance PolyVec f (f Int32) where
  vectorize x = mempty {arrayInt32 = V.singleton x}
  devectorizeIncremental v
    | V.null int32s = Left "ran out of int32s"
    | otherwise = Right (V.head int32s, v {arrayInt32 = V.tail int32s})
    where
      int32s = arrayInt32 v
  vlengths = const . const $ mempty {nInt32 = 1}

instance PolyVec f (f Int64) where
  vectorize x = mempty {arrayInt64 = V.singleton x}
  devectorizeIncremental v
    | V.null int64s = Left "ran out of int64s"
    | otherwise = Right (V.head int64s, v {arrayInt64 = V.tail int64s})
    where
      int64s = arrayInt64 v
  vlengths = const . const $ mempty {nInt64 = 1}

instance PolyVec f (f Word8) where
  vectorize x = mempty {arrayWord8 = V.singleton x}
  devectorizeIncremental v
    | V.null word8s = Left "ran out of word8s"
    | otherwise = Right (V.head word8s, v {arrayWord8 = V.tail word8s})
    where
      word8s = arrayWord8 v
  vlengths = const . const $ mempty {nWord8 = 1}

instance PolyVec f (f Word16) where
  vectorize x = mempty {arrayWord16 = V.singleton x}
  devectorizeIncremental v
    | V.null word16s = Left "ran out of word16s"
    | otherwise = Right (V.head word16s, v {arrayWord16 = V.tail word16s})
    where
      word16s = arrayWord16 v
  vlengths = const . const $ mempty {nWord16 = 1}

instance PolyVec f (f Word32) where
  vectorize x = mempty {arrayWord32 = V.singleton x}
  devectorizeIncremental v
    | V.null word32s = Left "ran out of word32s"
    | otherwise = Right (V.head word32s, v {arrayWord32 = V.tail word32s})
    where
      word32s = arrayWord32 v
  vlengths = const . const $ mempty {nWord32 = 1}

instance PolyVec f (f Word64) where
  vectorize x = mempty {arrayWord64 = V.singleton x}
  devectorizeIncremental v
    | V.null word64s = Left "ran out of word64s"
    | otherwise = Right (V.head word64s, v {arrayWord64 = V.tail word64s})
    where
      word64s = arrayWord64 v
  vlengths = const . const $ mempty {nWord64 = 1}

instance PolyVec f (f Float) where
  vectorize x = mempty {arrayFloat = V.singleton x}
  devectorizeIncremental v
    | V.null floats = Left "ran out of floats"
    | otherwise = Right (V.head floats, v {arrayFloat = V.tail floats})
    where
      floats = arrayFloat v
  vlengths = const . const $ mempty {nFloat = 1}

instance PolyVec f (f Double) where
  vectorize x = mempty {arrayDouble = V.singleton x}
  devectorizeIncremental v
    | V.null doubles = Left "ran out of doubles"
    | otherwise = Right (V.head doubles, v {arrayDouble = V.tail doubles})
    where
      doubles = arrayDouble v
  vlengths = const . const $ mempty {nDouble = 1}
