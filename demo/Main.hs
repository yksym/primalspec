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

data VMEvent
    = Coin
    | Juice
    | Bingo
    | Fill Int
    | Maintanance
    | Disposal VMState
    deriving (Data, Eq, Ord, Read, Show)

makeLenses ''VMState

type Process = ProcExp VMState VMEvent

initState :: VMState
initState = VMState {
    _name   = "fst"
  , _coin   = 1
  , _juice  = 2
}

complexCalc :: UseSuchThat => Int -> Int
complexCalc n = s_t_ "what is n?" $ \n' -> n' == (n + 1)

entry :: UseSuchThat => Process
entry = inService <|> maintenance

maintenance :: UseSuchThat => Process
maintenance = Load $ \s -> Maintanance --> nop %-> (entry |=| Disposal s --> nop %-> Skip)

outOfService :: UseSuchThat => Process
outOfService = Fill ?-> \n -> do { coin .= complexCalc 1 ; juice += n; } %-> inService

inService :: UseSuchThat => Process
inService = Load $ \s -> outOfService |=| if
  | s ^. juice == 1   -> Coin --> do { coin += 1; } %-> Juice --> do {juice -= 1; } %-> inService
  | s ^. juice >  1   -> Coin --> do { coin += 1; } %-> Juice --> do {juice -= 1; } %->
                               ( inService |=| Bingo --> nop %-> Juice --> do {juice -= 1; } %-> inService )
  | otherwise         -> Stop

testcase :: UseSuchThat => [VMEvent]
testcase = [Coin, Juice, Coin, Juice, (2::Int) *!* Fill 1, Coin, Juice]

testRepl :: IO ()
testRepl = useStdInSuchThat $ repl initState entry

--testAuto :: IO ()
--testAuto = do
--    ref <- newIORef ""
--    useIORefSuchThat ref $ void $ batchStep initState entry testcase

main :: IO ()
main = testRepl
--main = testAuto

