{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import PrimalSpec
import Control.Lens
--import Data.IORef


-- VM: vendor machine
data VMState = VMState {
    _name     :: String
  , _coin     :: Int
  , _juice    :: Int
} deriving (Data, Eq, Ord, Read, Show)

makeLenses ''VMState

data VMEvent
    = Coin
    | Juice
    | Bingo
    | Fill Int
    | Maintanance
    | Disposal VMState
    deriving (Data, Eq, Ord, Read, Show)

makePrisms ''VMEvent

type Process = ProcExp VMState VMEvent

initState :: VMState
initState = VMState {
    _name   = "fst"
  , _coin   = 1
  , _juice  = 2
}

entry :: Process
entry = inService <|> maintenance

maintenance :: Process
maintenance = Load $ \s -> Maintanance --> nop %-> (entry |=| Disposal s --> nop %-> Skip)

outOfService :: Process
outOfService = _Fill ?-> \n -> Just $ do { coin .= 10 ; juice += n; } %-> inService

inService :: Process
inService = Load $ \s -> outOfService |=| if
  | s ^. juice == 1   -> Coin --> do { coin += 1; } %-> Juice --> do {juice -= 1; } %-> inService
  | s ^. juice >  1   -> Coin --> do { coin += 1; } %-> Juice --> do {juice -= 1; } %->
                               ( inService |=| Bingo --> nop %-> Juice --> do {juice -= 1; } %-> inService )
  | otherwise         -> Stop

testcase :: [VMEvent]
testcase = [Coin, Juice, Coin, Juice, Fill 1, Coin, Juice]

testRepl :: IO ()
testRepl = repl initState entry

testAuto :: IO ()
testAuto = void $ batchStep initState entry testcase

main :: IO ()
--main = testRepl
main = testAuto

