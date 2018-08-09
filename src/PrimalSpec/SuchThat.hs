{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module PrimalSpec.SuchThat
( SomeSuchThat
, StdInSuchThatImpl(..)
--, IORefSuchThatImpl(..)
, UseSuchThat
, useSuchThat
, s_t_
--, s_t_solve
--, (|?|)
) where

import Data.Reflection (Given, give, given)
import System.IO.Unsafe (unsafePerformIO)
--import Data.IORef
import PrimalSpec.Util
import Data.IORef

class SuchThat s where
    suchThat :: (Read a) => s -> String -> (a -> Bool) -> a
    hintSuchThat :: (Show a) => s -> a -> b -> b

data SomeSuchThat = forall s. SuchThat s => SomeSuchThat s

instance SuchThat SomeSuchThat where
    suchThat (SomeSuchThat s) = suchThat s
    hintSuchThat (SomeSuchThat s) = hintSuchThat s

data IORefSuchThatImpl = IORefSuchThatImpl (IORef String)

data StdInSuchThatImpl = StdInSuchThatImpl

instance SuchThat StdInSuchThatImpl where
  suchThat _ q p = unsafePerformIO $ askStdIn q p
  hintSuchThat _ _ = id

instance SuchThat IORefSuchThatImpl where
  suchThat (IORefSuchThatImpl ref) q p = unsafePerformIO $ askStringIORef ref q p
  hintSuchThat (IORefSuchThatImpl ref) q p = unsafePerformIO $ do
   writeIORef ref (show q)
   return p


type UseSuchThat = Given SomeSuchThat

s_t_ :: (UseSuchThat, Read a) => String -> (a -> Bool) -> a
s_t_ = suchThat (given :: SomeSuchThat)

useSuchThat :: SuchThat s => s -> (Given SomeSuchThat => r) -> r
useSuchThat = give . SomeSuchThat

