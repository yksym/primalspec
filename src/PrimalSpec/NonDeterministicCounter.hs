{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module PrimalSpec.NonDeterministicCounter
( readNDC
, UseNDC
, useNDC
, StdInNDCImpl(..)
) where

import PrimalSpec.Util
import Data.Reflection (Given, give, given)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

class NDC c where
    _readNDC :: c -> String -> Int

data SomeNDC = forall s. NDC s => SomeNDC s

instance NDC SomeNDC where
    _readNDC (SomeNDC s) = _readNDC s

data ConstNDCImpl = ConstNDCImpl Int

data StdInNDCImpl = StdInNDCImpl

-- 評価タイミングや評価回数のことを考えると使うの難しい・・・
data IORefNDDecImpl = IORefNDDecImpl (IORef Int)

instance NDC ConstNDCImpl where
  _readNDC (ConstNDCImpl n) _ = n

instance NDC StdInNDCImpl where
  _readNDC _ s = unsafePerformIO $ askStdIn s (>= 0)

instance NDC IORefNDDecImpl where
  _readNDC (IORefNDDecImpl ref) _ = unsafePerformIO $ do
    n <- askIORef ref "input [0-]" (>= 0)
    writeIORef ref $ if n > 0 then n - 1 else n
    return n

type UseNDC = Given SomeNDC

useNDC :: NDC s => s -> (Given SomeNDC => r) -> r
useNDC = give . SomeNDC

readNDC :: UseNDC => String -> Int
readNDC = _readNDC (given :: SomeNDC)

useIntIORef :: IORef a -> (Given (IORef a) => r) -> r
useIntIORef = give

type UseIntIORef = Given (IORef Int)

setIORefNDDec :: UseIntIORef => Int -> a -> a
setIORefNDDec  n a = unsafePerformIO $ do
    writeIORef given n
    return a

