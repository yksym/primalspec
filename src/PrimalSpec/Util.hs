{-# LANGUAGE ExistentialQuantification, ConstraintKinds, FlexibleContexts, RankNTypes #-}

module PrimalSpec.Util
( SomeDPrint
, UseDPrint
, useDPrint
, useStdoutDPrint
, useDummyDPrint
, dprint'
, dprintM'
, dprint
, dprintM
) where

import Data.Reflection (Given, give, given)
import Debug.Trace
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad(when)
import System.Environment

class DPrint s where
    _dprint  :: (Show a) => s -> a -> b -> b
    _dprintM :: (Show a, Monad m) => s -> a -> m ()
    _dprintM s x = _dprint s x $ return ()

data SomeDPrint = forall s. DPrint s => SomeDPrint s

instance DPrint SomeDPrint where
    _dprint (SomeDPrint s) = _dprint s

data StdoutDPrintImpl = StdoutDPrintImpl
data DummyDPrintImpl = DummyDPrintImpl

instance DPrint StdoutDPrintImpl where
  _dprint _ = traceShow

instance DPrint DummyDPrintImpl where
  _dprint _ _= id

type UseDPrint = Given SomeDPrint

readEnv :: String -> IO Int
readEnv k = getEnv k >>= \n -> return $ read n

dprint' :: (Show a) => a -> b -> b
dprint' x y = unsafePerformIO $ do
    n <- readEnv "DEBUG"
    when (n == 1) $ print x
    return y

dprintM' :: (Show a, Monad m) => a -> m ()
dprintM' x = dprint' x $ return ()

dprint :: (UseDPrint, Show a) => a -> b -> b
dprint = _dprint (given :: SomeDPrint)

dprintM :: (UseDPrint, Show a, Monad m) => a -> m ()
dprintM = _dprintM (given :: SomeDPrint)

useDPrint :: DPrint s => s -> (Given SomeDPrint => r) -> r
useDPrint = give . SomeDPrint

useDummyDPrint :: (Given SomeDPrint => r) -> r
useDummyDPrint = useDPrint DummyDPrintImpl

useStdoutDPrint :: (Given SomeDPrint => r) -> r
useStdoutDPrint = useDPrint StdoutDPrintImpl

