{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Writeup of thoughts on design decisions.
module PolyVec.Writeup where

import qualified GHC.Generics
import Generics.SOP

import PolyVec.Class ( PolyVec(..), Arrays(..), ArrayLengths(..), devectorize' )

import Data.Int ( Int8, Int16, Int32, Int64 )
import Data.List ( intercalate )
import Data.Word ( Word8, Word16, Word32, Word64 )
import qualified Data.Vector as V
--import Data.SBV ( SBV )


-- | The goal is to write polymorphic haskell code that can be evaluted
-- with different types of typed expressions.
-- For example consider two types of expressions.
-- First, a symbolic expression with a 'Num' instance:
data Expr a where
  -- symbolics
  Sym :: String -> Expr a
  Constant :: a -> Expr a

  -- Num stuff
  Mul :: Num a => Expr a -> Expr a -> Expr a
  Add :: Num a => Expr a -> Expr a -> Expr a
  Sub :: Num a => Expr a -> Expr a -> Expr a
  Abs :: Num a => Expr a -> Expr a
  Signum :: Num a => Expr a -> Expr a
  Negate :: Num a => Expr a -> Expr a

  -- 'SymEq' stuff
  ExprEq :: Eq a => Expr a -> Expr a -> Expr Bool
  ExprNeq :: Eq a => Expr a -> Expr a -> Expr Bool

instance Show (Expr a) where
  show (Sym x) = x
  show (Constant _) = "(dontmindme)"

  show (Mul x y) = "(" ++ show x ++ ") * (" ++ show y ++ ")"
  show (Add x y) = "(" ++ show x ++ ") + (" ++ show y ++ ")"
  show (Sub x y) = "(" ++ show x ++ ") - (" ++ show y ++ ")"
  show (Abs x) = "abs(" ++ show x ++ ")"
  show (Signum x) = "signum(" ++ show x ++ ")"
  show (Negate x) = "negate(" ++ show x ++ ")"

  -- 'SymEq' stuff
  show (ExprEq x y) = "(" ++ show x ++ ") == (" ++ show y ++ ")"
  show (ExprNeq x y) = "(" ++ show x ++ ") /= (" ++ show y ++ ")"

instance Num a => Num (Expr a) where
  (*) = Mul
  (+) = Add
  (-) = Sub
  abs = Abs
  signum = Signum
  fromInteger = Constant . fromInteger
  negate = negate

-- | Second: a newtype wrapper for evaluating natively.
newtype Native a = Native a deriving (Num, Eq)
instance Show a => Show (Native a) where
  showsPrec k (Native x) = showsPrec k x

-- | A simple example function which can be evaluated with both.
simpleFun :: Num a => a -> a -> a
simpleFun x y = x * y

-- | you can run this numerically with 'Native'
simpleFunNative :: Native Double -> Native Double -> Native Double
simpleFunNative = simpleFun

-- | you can run this symbolically with 'Expr'
simpleFunSym :: Expr Double -> Expr Double -> Expr Double
simpleFunSym = simpleFun


-- | So far so good, right? Let's make it more complicated.
-- We want to use functions that have explicit data types, not type classes.
explicitFun :: Num (f Double) => f Double -> f Double -> f Double
explicitFun x y = x * y

-- | you can run this numerically with 'Native'
explicitFunNative :: Native Double -> Native Double -> Native Double
explicitFunNative = explicitFun

-- | you can run this symbolically with 'Expr'
explicitFunSym :: Expr Double -> Expr Double -> Expr Double
explicitFunSym = explicitFun


-- | So far we've only really been using 'Double'.
-- It would be useful to be able to work with 'Bool' too.
-- What would `==` look like?
eqFun :: Eq (f Double) => f Double -> f Double -> Bool
eqFun x y = x == y

-- | We can run this numerically with 'Native'
eqFunNative :: Native Double -> Native Double -> Bool
eqFunNative = eqFun

-- | But we cannot run this symbolically with 'Expr'.
-- Because 'Expr' has the 'Sym' constructor it doesn't know
-- whether two 'Expr's are equal until they are instantiated.
-- To workaround this we'll implement our symbolic equality class
-- `SymEq` and instance 'Native' and 'Expr'
class SymEq f where
  (.==) :: Eq a => f a -> f a -> f Bool
  (./=) :: Eq a => f a -> f a -> f Bool

instance SymEq Expr where
  (.==) = ExprEq
  (./=) = ExprNeq

instance SymEq Native where
  x .== y = Native (x == y)
  x ./= y = Native (x /= y)


-- | Alright! We are on our way to strongly typed symbolic type class hierarchy
-- which can also be evaluated natively.
-- What comes next? Writing some real-world code.
data Foo f = Foo (f Double) (f Bool) deriving (GHC.Generics.Generic)
instance Generic (Foo f)
instance PolyVec f (Foo f)

data RealWorldInputs f = RealWorldInputs (f Double) (Foo f) (f Double) deriving (GHC.Generics.Generic)
instance Generic (RealWorldInputs f)
instance PolyVec f (RealWorldInputs f)

data RealWorldOutputs f = RealWorldOutputs (f Double) (f Bool) deriving (GHC.Generics.Generic)
instance Generic (RealWorldOutputs f)
instance PolyVec f (RealWorldOutputs f)

realWorldFunction :: (Num (f Double), SymEq f) => RealWorldInputs f -> RealWorldOutputs f
realWorldFunction (RealWorldInputs x (Foo w _) y) = RealWorldOutputs (x * y + w) (x .== (y + w))

-- | We can run this function natively.
realWorldFunctionNative :: RealWorldInputs Native -> RealWorldOutputs Native
realWorldFunctionNative = realWorldFunction

-- | We can run this function symbolically.
realWorldFunctionSym :: RealWorldInputs Expr -> RealWorldOutputs Expr
realWorldFunctionSym = realWorldFunction

-- | Great! Now the reason we've been jumping through these hoops is
-- for doing codegen. How can we convert 'realWorldFunctionSym'
-- to a C function?
-- Use the 'toSymbolic' function to get a symbolic tree representing the function.
-- Then the usual reify magic.
toGraph :: forall x y .
           ( PolyVec Expr (x Expr)
           )
        => (x Expr -> y Expr) -> y Expr
toGraph fun = fun inputs
  where
    inputs :: x Expr
    inputs = case devectorize' inputArrays of
      Right r -> r
      Left err -> error $ "internal error, ya goon (" ++ err ++ ")"

    inputArrays :: Arrays Expr
    inputArrays = toSymbolicInputArrays $ vlengths (Proxy :: Proxy Expr) (Proxy :: Proxy (x Expr))
      where
        toSymbolicInputArrays :: ArrayLengths Word64 -> Arrays Expr
        toSymbolicInputArrays ns =
          Arrays
          { arrayBool   = toSyms "b"   (nBool   ns)
          , arrayInt8   = toSyms "i8"  (nInt8   ns)
          , arrayInt16  = toSyms "i16" (nInt16  ns)
          , arrayInt32  = toSyms "i32" (nInt32  ns)
          , arrayInt64  = toSyms "i64" (nInt64  ns)
          , arrayWord8  = toSyms "u8"  (nWord8  ns)
          , arrayWord16 = toSyms "u16" (nWord16 ns)
          , arrayWord32 = toSyms "u32" (nWord32 ns)
          , arrayWord64 = toSyms "u64" (nWord64 ns)
          , arrayFloat  = toSyms "f"   (nFloat  ns)
          , arrayDouble = toSyms "d"   (nDouble ns)
          }
          where
            toSyms :: forall a . String -> Word64 -> V.Vector (Expr a)
            toSyms name n = V.fromList $ map toSym ks
              where
                ks = take (fromIntegral n) [0..]

                toSym :: Int -> Expr a
                toSym k = Sym (name ++ "_" ++ show k)


-- | let's try running it on our "real world function"
realWorldGraph :: RealWorldOutputs Expr
realWorldGraph = toGraph realWorldFunction

-- | and for code-generation purposes we can turn it into arrays
realWorldGraphAsArrays :: Arrays Expr
realWorldGraphAsArrays = vectorize realWorldGraph

-- | yeah i don't have a pretty show instance
uglyShow :: ( Show (f Bool)
            , Show (f Int8)
            , Show (f Int16)
            , Show (f Int32)
            , Show (f Int64)
            , Show (f Word8)
            , Show (f Word16)
            , Show (f Word32)
            , Show (f Word64)
            , Show (f Float)
            , Show (f Double)
            )
         => Arrays f -> String
uglyShow x =
  intercalate "\n"
  [ "Arrays"
  , "{ arrayBool    = " ++ show (V.toList (arrayBool   x))
  , ", arrayInt8    = " ++ show (V.toList (arrayInt8   x))
  , ", arrayInt16   = " ++ show (V.toList (arrayInt16  x))
  , ", arrayInt32   = " ++ show (V.toList (arrayInt32  x))
  , ", arrayInt64   = " ++ show (V.toList (arrayInt64  x))
  , ", arrayWord8   = " ++ show (V.toList (arrayWord8  x))
  , ", arrayWord16  = " ++ show (V.toList (arrayWord16 x))
  , ", arrayWord32  = " ++ show (V.toList (arrayWord32 x))
  , ", arrayWord64  = " ++ show (V.toList (arrayWord64 x))
  , ", arrayFloat   = " ++ show (V.toList (arrayFloat  x))
  , ", arrayDoubles = " ++ show (V.toList (arrayDouble x))
  , "}"
  ]

-- | so here's the result
uglyRealWorldGrahAsArrays :: String
uglyRealWorldGrahAsArrays = uglyShow realWorldGraphAsArrays


-- down here, just an idea for discarding Native types to make things easier to work with?
--type family S f a :: * where
--  S SBV a = SBV a
--  S Native a = a
