{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import PrimalSpec
import Control.Lens
import Data.IORef


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


complexCalc :: UseSuchThat => Int -> Int
complexCalc n = s_t_ "what is n?" $ \n' -> n' == (n + 1)


vm :: UseSuchThat => VMState -> Process
vm s = s *?* if
  | s ^. juice == 0   -> Fill ?-> \n -> vm (s &~ do { vmCoin .= complexCalc 1 ; juice += n; })

  | s ^. juice == 1   -> Coin --> Juice --> vm (s &~ do { vmCoin += 1; juice -= 1; })
                         |=| Fill ?-> \n -> vm (s &~ do { vmCoin .= complexCalc 3; juice += n; })

  | s ^. juice >  1   -> Coin --> Juice -->
                               (   vm (s &~ do { vmCoin += 1; juice -= 1; })
                                   |=| Bingo --> Juice --> vm (s &~ do { vmCoin += 1; juice -= 2; })
                               )
                         |=| Fill ?-> \n -> vm (s &~ do { vmCoin .= complexCalc 10; juice += n; })

  | otherwise         -> Stop

testcase :: UseSuchThat => Process
testcase = Coin --> Juice --> Coin --> Juice --> (2::Int) *!* Fill 1 --> Coin --> Juice --> Skip


testRepl :: IO ()
testRepl = useStdInSuchThat $ repl $ entry <||> testcase

testAuto :: IO ()
testAuto = do
    ref <- newIORef ""
    useIORefSuchThat ref $ void $ autoStep $ entry <||> testcase

main :: IO ()
main = testRepl
--main = testAuto

