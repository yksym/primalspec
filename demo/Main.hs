{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import PrimalSpec
import Control.Lens


data VMState = VMState {
    _vmCoin   :: Int
  , _juice    :: Int
} deriving Show

data VMEvent
    = Coin
    | Juice
    | Bingo
    | Fill Int
    deriving (Data, Eq, Ord, Read, Show)

makeLenses ''VMState

type Process = ProcExp VMEvent

entry :: UseSuchThat => Process
entry = vm VMState {
    _vmCoin  = 1
  , _juice   = 2
}


complexCalc :: UseSuchThat => VMState -> Int
complexCalc s = s_t_ "what is n?" (\n -> (n + (s ^. vmCoin)) `mod` 4 == 1)



vm :: UseSuchThat => VMState -> Process
vm s
  | s ^. juice == 0           =  Fill ?-> \n -> vm (s &~ do { vmCoin .= complexCalc s; juice += n; })

  | s ^. juice == 1           =  Coin --> Juice --> vm (s &~ do { vmCoin += 1; juice -= 1; })
                             |=| Fill ?-> \n -> vm (s &~ do { vmCoin .= complexCalc s; juice += n; })

  | s ^. juice >  1           =  Coin --> Juice -->
                                   (   vm (s &~ do { vmCoin += 1; juice -= 1; })
                                   |=| Bingo --> Juice --> vm (s &~ do { vmCoin += 1; juice -= 2; })
                                   )
                             |=| Fill ?-> \n -> vm (s &~ do { vmCoin .= complexCalc s; juice += n; })

  | otherwise                 = Stop

test :: Process
test = Coin --> Juice --> Coin --> Juice --> Fill 1 --> Coin --> Juice --> Skip

main :: IO ()
--main = useSuchThat StdInSuchThatImpl $ repl entry
--main = useSuchThat StdInSuchThatImpl $ repl $ entry <||> test
main = print $ useSuchThat StdInSuchThatImpl $ autoStep $ entry <||> test

