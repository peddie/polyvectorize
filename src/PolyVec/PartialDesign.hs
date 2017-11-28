{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- for tests
{-# LANGUAGE DeriveGeneric #-}

-- | First pass at a polyvectorize idea.
-- This is a proof of concept implementation to think about a final design.
-- This file is trying to address supporting both symbolic and native types.
module PolyVec.PartialDesign where
--       ( PolyVec(..)
--       , Arrays(..), ArrayLengths(..)
--       , GPolyVec(..)
--       ) where

import GHC.Generics

--import Data.Either ( partitionEithers )
import Data.Int ( Int8, Int16, Int32, Int64 )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Data.Proxy ( Proxy(..) )
import Data.Word ( Word8, Word16, Word32, Word64 )
--import Text.Printf ( printf )

import Data.SBV ( SBV )


data Arrays (f :: * -> *)
  = Arrays
    { arrayInt8   :: Vector (f Int8)
--    , arrayInt16  :: Vector (f Int16)
--    , arrayInt32  :: Vector (f Int32)
--    , arrayInt64  :: Vector (f Int64)
--    , arrayWord8  :: Vector (f Word8)
--    , arrayWord16 :: Vector (f Word16)
--    , arrayWord32 :: Vector (f Word32)
--    , arrayWord64 :: Vector (f Word64)
--    , arrayFloat  :: Vector (f Float)
    , arrayDouble :: Vector (f Double)
    }
instance Show (Arrays Id) where
  show x =
    "Arrays " ++
    "{ arrayInt8 = " ++ show (arrayInt8 x) ++
--    ", arrayInt16 = " ++ show (arrayInt16 x) ++
--    ", arrayInt32 = " ++ show (arrayInt32 x) ++
--    ", arrayInt64 = " ++ show (arrayInt64 x) ++
--    ", arrayWord8 = " ++ show (arrayWord8 x) ++
--    ", arrayWord16 = " ++ show (arrayWord16 x) ++
--    ", arrayWord32 = " ++ show (arrayWord32 x) ++
--    ", arrayWord64 = " ++ show (arrayWord64 x) ++
--    ", arrayFloat = " ++ show (arrayFloat x) ++
    ", arrayDouble = " ++ show (arrayDouble x) ++
    "}"

emptyArrays :: Arrays f
emptyArrays = 
  Arrays
  { arrayInt8   = V.empty
--  , arrayInt16  = V.empty
--  , arrayInt32  = V.empty
--  , arrayInt64  = V.empty
--  , arrayWord8  = V.empty
--  , arrayWord16 = V.empty
--  , arrayWord32 = V.empty
--  , arrayWord64 = V.empty
--  , arrayFloat  = V.empty
  , arrayDouble = V.empty
  }

data ArrayLengths
  = ArrayLengths
    { nInt8   :: Int
--    , nInt16  :: Int
--    , nInt32  :: Int
--    , nInt64  :: Int
--    , nWord8  :: Int
--    , nWord16 :: Int
--    , nWord32 :: Int
--    , nWord64 :: Int
--    , nFloat  :: Int
    , nDouble :: Int
    } deriving Show


class PolyVec (a :: (* -> *) -> *) where
  vectorize :: a f -> Arrays f
  devectorize' :: Arrays f -> Either String (a f)
  vlengths :: Proxy a -> ArrayLengths

  default vectorize :: (Generic (a f), GPolyVec (Rep (a f)) f) => a f -> Arrays f
  vectorize x = gvectorize (from x)

  default devectorize' :: (Generic (a f), GPolyVec (Rep (a f)) f) => Arrays f -> Either String (a f)
  devectorize' x = to <$> gdevectorize x

--  default vlengths :: GPolyVec (Rep1 a) => Proxy a -> ArrayLengths
--  vlengths = const $ gvlengths (Proxy :: Proxy (Rep a)) (Proxy :: Proxy (Rep a))


class GPolyVec a f where
  gvectorize :: a p -> Arrays f
  gdevectorize :: Arrays f -> Either String (a p)
  gvlengths :: Proxy a -> Proxy f -> ArrayLengths



--class PolyVec (a :: *) where
--  vectorize :: a -> Arrays f
--  devectorize' :: Arrays f -> Either String a
--  vlengths :: Proxy a -> ArrayLengths
--
--  default vectorize :: (Generic a, GPolyVec (Rep a)) => a -> Arrays f
--  vectorize x = gvectorize (from x)
--
--  default devectorize' :: (Generic a, GPolyVec (Rep a)) => Arrays f -> Either String a
--  devectorize' x = fmap to (gdevectorize x)
--
--  default vlengths :: GPolyVec (Rep a) => Proxy a -> ArrayLengths
--  vlengths = const $ gvlengths (Proxy :: Proxy (Rep a))
--
--
--class GPolyVec (a :: * -> *) where
--  gvectorize :: a p -> Arrays f
--  gdevectorize :: Arrays f -> Either String (a p)
--  gvlengths :: Proxy a -> ArrayLengths

-- product type (concatination)
--instance (GPolyVec f, GPolyVec g) => GPolyVec (f :*: g) where
--  gvectorize (_f :*: _g) = undefined -- gvectorize f V.++ gvectorize g
--  gdevectorize v0s
--    | V.length v0s < n0 =
--      Left $ "gdevectorize (f :*: g): V.length v0s < vlength f0  (" ++
--             show (V.length v0s) ++ " < " ++ show n0 ++ ")"
--    | V.length v1 /= n1 =
--      Left $ "gdevectorize (f :*: g): V.length v1 /= vlength f1  (" ++
--             show (V.length v1) ++ " /= " ++ show n1 ++ ")"
--    | otherwise = case (ef0, ef1) of
--      (Left msg0, Left msg1) ->
--        Left $ "gdevectorize (f :*: g): errored on both sides: {" ++ msg0 ++ ", " ++ msg1 ++ "}"
--      (Left msg0, Right   _) ->
--        Left $ "gdevectorize (f :*: g): errored on left side: " ++ msg0
--      (Right   _, Left msg1) ->
--        Left $ "gdevectorize (f :*: g): errored on right side: " ++ msg1
--      (Right f0, Right f1) -> Right (f0 :*: f1)
--    where
--      ef0 = gdevectorize v0
--      ef1 = gdevectorize v1
--
--      n0 = gvlength (Proxy :: Proxy f)
--      n1 = gvlength (Proxy :: Proxy g)
--
--      (v0,v1) = V.splitAt n0 v0s
--
--  gvlength = const (nf + ng)
--    where
--      nf = gvlength (Proxy :: Proxy f)
--      ng = gvlength (Proxy :: Proxy g)

-- Metadata (constructor name, etc)
instance GPolyVec a f => GPolyVec (M1 i c a) f where
  gvectorize = gvectorize . unM1
  gdevectorize = fmap M1 . gdevectorize
  gvlengths = gvlengths . proxy
    where
      proxy :: Proxy (M1 i c a) -> Proxy a
      proxy = const Proxy

---- singleton
--instance GPolyVec Par1 where
--  gvectorize = V.singleton . unPar1
--  gdevectorize v = case V.toList v of
--    [] -> Left "gdevectorize Par1: got empty list"
--    [x] -> Right (Par1 x)
--    xs -> Left $ "gdevectorize Par1: got non-1 length: " ++ show (length xs)
--  gvlength = const 1

toArrayLengths :: Arrays f -> ArrayLengths
toArrayLengths x =
  ArrayLengths
  { nInt8   = V.length (arrayInt8   x)
--  , nInt16  = V.length (arrayInt16  x)
--  , nInt32  = V.length (arrayInt32  x)
--  , nInt64  = V.length (arrayInt64  x)
--  , nWord8  = V.length (arrayWord8  x)
--  , nWord16 = V.length (arrayWord16 x)
--  , nWord32 = V.length (arrayWord32 x)
--  , nWord64 = V.length (arrayWord64 x)
--  , nFloat  = V.length (arrayFloat x)
  , nDouble = V.length (arrayDouble x)
  }


allEmpty :: ArrayLengths -> Bool
allEmpty lengths =
  all (== 0)
  [ nInt8   lengths
--  , nInt16  lengths
--  , nInt32  lengths
--  , nInt64  lengths
--  , nWord8  lengths
--  , nWord16 lengths
--  , nWord32 lengths
--  , nWord64 lengths
--  , nFloat  lengths
  , nDouble lengths
  ]

---- data with no fields
--instance GPolyVec U1 where
--  gvectorize = const emptyArrays
--  gdevectorize v
--    | allEmpty lengths = Right U1
--    | otherwise = Left $ "gdevectorize U1: got non-null arrays, lengths: " ++ show lengths
--    where
--      lengths = toArrayLengths v
--  gvlengths =
--    const
--    ArrayLengths
--    { nInt8   = 0
--    , nInt16  = 0
--    , nInt32  = 0
--    , nInt64  = 0
--    , nWord8  = 0
--    , nWord16 = 0
--    , nWord32 = 0
--    , nWord64 = 0
--    , nFloat  = 0
--    , nDouble = 0
--    }
--
--instance PolyVec f => GPolyVec (Rec1 f) where
--  gvectorize = vectorize . unRec1
--  gdevectorize = fmap Rec1 . devectorize'
--  gvlengths = vlengths . proxy
--    where
--      proxy :: Proxy (Rec1 f) -> Proxy f
--      proxy = const Proxy



instance GPolyVec (Rec0 (f Int8)) f where
  gvectorize (K1 x) = 
    Arrays
    { arrayInt8   = V.singleton x
--    , arrayInt16  = V.empty
--    , arrayInt32  = V.empty
--    , arrayInt64  = V.empty
--    , arrayWord8  = V.empty
--    , arrayWord16 = V.empty
--    , arrayWord32 = V.empty
--    , arrayWord64 = V.empty
--    , arrayFloat  = V.empty
    , arrayDouble = V.empty
    }
--  gdevectorize = fmap K1 . devectorize'
--  gvlengths = vlengths . proxy
--    where
--      proxy :: Proxy (Rec0 a) -> Proxy a
--      proxy = const Proxy

---- composition
--instance (PolyVec f, GPolyVec g) => GPolyVec (f :.: g) where
--  gvectorize = V.concatMap gvectorize . vectorize . unComp1
--  gdevectorize v = case partitionEithers (V.toList evs) of
--    ([], vs) -> fmap Comp1 (devectorize' (V.fromList vs))
--    (bad, good) -> Left $ printf "gdevectorize (f :.: g): got %d failures and %d successes"
--                          (length bad) (length good)
--    where
--      kf = vlength (Proxy :: Proxy f)
--      kg = gvlength (Proxy :: Proxy g)
--
--      --evs :: V.Vector (Either String (g a))
--      evs = fmap gdevectorize (splitsAt kg kf v {-:: Vec nf (Vec ng a)-} )
--
--  gvlength = const (nf * ng)
--    where
--      nf = vlength (Proxy :: Proxy f)
--      ng = gvlength (Proxy :: Proxy g)

---- break a vector jOuter vectors, each of length kInner
--splitsAt' :: Int -> Int -> V.Vector a -> [V.Vector a]
--splitsAt' 0 jOuter v
--  | V.null v = replicate jOuter V.empty
--  | otherwise = error $ "splitsAt 0 " ++ show jOuter ++ ": got non-zero vector"
--splitsAt' kInner 0 v
--  | V.null v = []
--  | otherwise = error $ "splitsAt " ++ show kInner ++ " 0: leftover vector of length: " ++ show (V.length v)
--splitsAt' kInner jOuter v
--  | kv0 < kInner =
--    error $ "splitsAt " ++ show kInner ++ " " ++ show jOuter ++ ": " ++ "ran out of vector input"
--  | otherwise = v0 : splitsAt' kInner (jOuter - 1) v1
--  where
--    kv0 = V.length v0
--    (v0,v1) = V.splitAt kInner v
--
---- break a vector jOuter vectors, each of length kInner
--splitsAt :: Int -> Int -> V.Vector a -> V.Vector (V.Vector a)
--splitsAt k j = V.fromList . splitsAt' k j

newtype Id a = Id a deriving (Show, Generic, Generic1)
--instance PolyVec (f Int8) where
--  vectorize x = emptyArrays {arrayInt8 = V.singleton x}
--  devectorize' = undefined
--  vlengths = undefined
--
--data WatWat (f :: * -> *) = WatWat (f Int8) deriving (Generic, Generic1)
data Foo a = Foo a deriving (Show, Generic, Generic1)

data WatWat f
  = WatWat
    { wat1 :: Foo (f Int8)
--    , wat0 :: f Int16
    } deriving (Generic)
--instance PolyVec WatWat

watwat :: WatWat Id
watwat = WatWat (Foo (Id 2)) -- (Id 3)

tawtaw :: Rep (WatWat Id) p
tawtaw = from watwat

go :: Arrays Id
go = gvectorize tawtaw

--go :: IO ()
--go = print (vectorize (WatWat (Id 22)) :: Arrays Id)
