{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module PolyVec.Class
       ( Vectorize(..)
       , GVectorize(..)
       ) where

import GHC.Generics

import Data.Either ( partitionEithers )
import qualified Data.Vector as V
import Data.Proxy ( Proxy(..) )
import Text.Printf ( printf )

-- | fmap f == devectorize . (V.map f) . vectorize
class Applicative f => Vectorize (f :: * -> *) where
  vectorize :: f a -> V.Vector a
  devectorize' :: V.Vector a -> Either String (f a)
  vlength :: Proxy f -> Int

  default vectorize :: (Generic1 f, GVectorize (Rep1 f)) => f a -> V.Vector a
  vectorize f = gvectorize (from1 f)

  default devectorize' :: (Generic1 f, GVectorize (Rep1 f)) => V.Vector a -> Either String (f a)
  devectorize' f = fmap to1 (gdevectorize f)

  default vlength :: GVectorize (Rep1 f) => Proxy f -> Int
  vlength = const $ gvlength (Proxy :: Proxy (Rep1 f))


class GVectorize (f :: * -> *) where
  gvectorize :: f a -> V.Vector a
  gdevectorize :: V.Vector a -> Either String (f a)
  gvlength :: Proxy f -> Int

-- product type (concatination)
instance (GVectorize f, GVectorize g) => GVectorize (f :*: g) where
  gvectorize (f :*: g) = gvectorize f V.++ gvectorize g
  gdevectorize v0s
    | V.length v0s < n0 =
      Left $ "gdevectorize (f :*: g): V.length v0s < vlength f0  (" ++
             show (V.length v0s) ++ " < " ++ show n0 ++ ")"
    | V.length v1 /= n1 =
      Left $ "gdevectorize (f :*: g): V.length v1 /= vlength f1  (" ++
             show (V.length v1) ++ " /= " ++ show n1 ++ ")"
    | otherwise = case (ef0, ef1) of
      (Left msg0, Left msg1) ->
        Left $ "gdevectorize (f :*: g): errored on both sides: {" ++ msg0 ++ ", " ++ msg1 ++ "}"
      (Left msg0, Right   _) ->
        Left $ "gdevectorize (f :*: g): errored on left side: " ++ msg0
      (Right   _, Left msg1) ->
        Left $ "gdevectorize (f :*: g): errored on right side: " ++ msg1
      (Right f0, Right f1) -> Right (f0 :*: f1)
    where
      ef0 = gdevectorize v0
      ef1 = gdevectorize v1

      n0 = gvlength (Proxy :: Proxy f)
      n1 = gvlength (Proxy :: Proxy g)

      (v0,v1) = V.splitAt n0 v0s

  gvlength = const (nf + ng)
    where
      nf = gvlength (Proxy :: Proxy f)
      ng = gvlength (Proxy :: Proxy g)

-- Metadata (constructor name, etc)
instance GVectorize f => GVectorize (M1 i c f) where
  gvectorize = gvectorize . unM1
  gdevectorize = fmap M1 . gdevectorize
  gvlength = gvlength . proxy
    where
      proxy :: Proxy (M1 i c f) -> Proxy f
      proxy = const Proxy

-- singleton
instance GVectorize Par1 where
  gvectorize = V.singleton . unPar1
  gdevectorize v = case V.toList v of
    [] -> Left "gdevectorize Par1: got empty list"
    [x] -> Right (Par1 x)
    xs -> Left $ "gdevectorize Par1: got non-1 length: " ++ show (length xs)
  gvlength = const 1

-- data with no fields
instance GVectorize U1 where
  gvectorize = const V.empty
  gdevectorize v
    | V.null v = Right U1
    | otherwise = Left $ "gdevectorize U1: got non-null vector, length: " ++ show (V.length v)
  gvlength = const 0

-- Constants, additional parameters, and rank-1 recursion
instance Vectorize f => GVectorize (Rec1 f) where
  gvectorize = vectorize . unRec1
  gdevectorize = fmap Rec1 . devectorize'
  gvlength = vlength . proxy
    where
      proxy :: Proxy (Rec1 f) -> Proxy f
      proxy = const Proxy

-- composition
instance (Vectorize f, GVectorize g) => GVectorize (f :.: g) where
  gvectorize = V.concatMap gvectorize . vectorize . unComp1
  gdevectorize v = case partitionEithers (V.toList evs) of
    ([], vs) -> fmap Comp1 (devectorize' (V.fromList vs))
    (bad, good) -> Left $ printf "gdevectorize (f :.: g): got %d failures and %d successes"
                          (length bad) (length good)
    where
      kf = vlength (Proxy :: Proxy f)
      kg = gvlength (Proxy :: Proxy g)

      --evs :: V.Vector (Either String (g a))
      evs = fmap gdevectorize (splitsAt kg kf v {-:: Vec nf (Vec ng a)-} )

  gvlength = const (nf * ng)
    where
      nf = vlength (Proxy :: Proxy f)
      ng = gvlength (Proxy :: Proxy g)

-- break a vector jOuter vectors, each of length kInner
splitsAt' :: Int -> Int -> V.Vector a -> [V.Vector a]
splitsAt' 0 jOuter v
  | V.null v = replicate jOuter V.empty
  | otherwise = error $ "splitsAt 0 " ++ show jOuter ++ ": got non-zero vector"
splitsAt' kInner 0 v
  | V.null v = []
  | otherwise = error $ "splitsAt " ++ show kInner ++ " 0: leftover vector of length: " ++ show (V.length v)
splitsAt' kInner jOuter v
  | kv0 < kInner =
    error $ "splitsAt " ++ show kInner ++ " " ++ show jOuter ++ ": " ++ "ran out of vector input"
  | otherwise = v0 : splitsAt' kInner (jOuter - 1) v1
  where
    kv0 = V.length v0
    (v0,v1) = V.splitAt kInner v

-- break a vector jOuter vectors, each of length kInner
splitsAt :: Int -> Int -> V.Vector a -> V.Vector (V.Vector a)
splitsAt k j = V.fromList . splitsAt' k j
