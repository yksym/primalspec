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

class SuchThat s where
    suchThat :: (Read a) => s -> String -> (a -> Bool) -> a

data SomeSuchThat = forall s. SuchThat s => SomeSuchThat s

instance SuchThat SomeSuchThat where
    suchThat (SomeSuchThat s) = suchThat s

--data IORefSuchThatImpl = IORefSuchThatImpl (IORef String)

data StdInSuchThatImpl = StdInSuchThatImpl

instance SuchThat StdInSuchThatImpl where
  suchThat _ q p = unsafePerformIO $ askStdIn q p

--instance SuchThat IORefSuchThatImpl where
--  suchThat (IORefSuchThatImpl ref) q p = unsafePerformIO $ askStringIORef ref q p


type UseSuchThat = Given SomeSuchThat

s_t_ :: (UseSuchThat, Read a) => String -> (a -> Bool) -> a
s_t_ = suchThat (given :: SomeSuchThat)

useSuchThat :: SuchThat s => s -> (Given SomeSuchThat => r) -> r
useSuchThat = give . SomeSuchThat

--type UseStringIORef = Given (IORef String)
--
--s_t_solve :: UseStringIORef => String -> a -> a
--s_t_solve  s a = unsafePerformIO $ do
--    writeIORef given s
--    return a
--
--(|?|) :: UseStringIORef => String -> a -> a
--(|?|) = s_t_solve
--
--infixr 0  |?|

