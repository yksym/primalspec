module PrimalSpec.SuchThat
( SomeSuchThat
, UseSuchThat
, useSuchThat
, useStdInSuchThat
, useIORefSuchThat
, s_t_
, hint_s_t_
, (*!*)
) where

import Data.Reflection (Given, give, given)
import System.IO.Unsafe (unsafePerformIO)
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

hint_s_t_ :: (UseSuchThat, Show a) => a -> b -> b
hint_s_t_ = hintSuchThat (given :: SomeSuchThat)

useSuchThat :: SuchThat s => s -> (Given SomeSuchThat => r) -> r
useSuchThat = give . SomeSuchThat

useStdInSuchThat :: (Given SomeSuchThat => r) -> r
useStdInSuchThat = useSuchThat StdInSuchThatImpl

useIORefSuchThat :: IORef String -> (Given SomeSuchThat => r) -> r
useIORefSuchThat ref = useSuchThat (IORefSuchThatImpl ref)

(*!*) :: (UseSuchThat, Show a) => a -> b -> b
(*!*) =  hint_s_t_

infixr 4 *!*
