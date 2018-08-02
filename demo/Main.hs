{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import PrimalSpec.ProcExp
import PrimalSpec.SuchThat
import PrimalSpec.NonDeterministicCounter
import Control.Lens
import Debug.Trace (trace, traceShow)
import Data.Data (Data)
import Data.Monoid ((<>))
import Text.Printf


data VMState = VMState {
    _vmCoin   :: Int
  , _juice    :: Int
} deriving Show

data VMEvent
    = Coin
    | Juice
    | Fill Int
    deriving (Data, Eq, Ord, Read, Show)

makeLenses ''VMState

initState = VMState {
    _vmCoin  = 1
  , _juice   = 2
}

satisfyInvariant :: VMState -> Bool
satisfyInvariant s  = s ^. vmCoin <= 10

type Process = ProcExp VMEvent

entry :: UseSuchThat => Process
entry = vm initState


complexCalc :: UseSuchThat => VMState -> Int
complexCalc s = s_t_ ("what is n?" <> show s) (\n -> (n + (s ^. vmCoin)) `mod` 4 == 1)

--dbg :: (Show s) => s -> a -> a
--dbg s = trace $ printf "\ndbg---------\n%s\n------------\n" $ show s

vm :: UseSuchThat => VMState -> Process
vm s = if
  | not (satisfyInvariant s) ->
    Stop
  | s ^. juice == 0 ->
    Fill ?-> \n -> vm (s &~ do { vmCoin .= complexCalc s; juice += n; })
  | s ^. juice == 1 ->
    Coin --> Juice --> vm (s &~ do { vmCoin += 1; juice -= 1; })
    |=| Fill ?-> \n -> vm (s &~ do { vmCoin .= complexCalc s; juice += n; })
  | s ^. juice >  1 ->
    Coin --> Juice --> (vm (s &~ do { vmCoin += 1; juice -= 1; }) |~| Juice --> (vm (s &~ do { vmCoin += 1; juice -= 2; })))
    |=| Fill ?-> \n -> vm (s &~ do { vmCoin .= complexCalc s; juice += n; })

test :: Process
test = Coin --> Juice --> Coin --> Juice --> Fill 1 --> Coin --> Juice --> Skip

main = useSuchThat StdInSuchThatImpl $ useNDC StdInNDCImpl
        --repl $ entry <||> test
        repl $ entry

