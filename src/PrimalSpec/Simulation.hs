{-# LANGUAGE GADTs, ExistentialQuantification, MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module PrimalSpec.Simulation
( repl
, autoStep
) where


import PrimalSpec.ProcExp
import PrimalSpec.Util
import Data.Monoid ((<>))
import Data.Maybe (isJust, fromJust)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import Data.Set as S

askEv :: (Data ev, Eq ev, Read ev, Show ev) => ProcExp ev -> IO ev
askEv p = askStdIn "input acceptable event" (isJust . tryStep p)


askEvByIdx :: (Data ev, Eq ev, Read ev, Show ev) => [ev] -> ProcExp ev -> IO ev
askEvByIdx evs p = do
    printEvents
    idx <- askStdIn ("input [1-" <> show (length evs) <> "]. 0 : perform another event.") (<= length evs)
    if
        | idx > 0 -> return $ evs !! (idx-1)
        | otherwise -> askEv p
    where
        printIndeces = putStr <$> [show n <> " : " | n <- [1..(length evs)]]
        printEvs     = print <$> evs
        printEvents  = sequence_ $ interleave printIndeces printEvs


repl :: (Data ev, Eq ev, Ord ev, Read ev, Show ev) => ProcExp ev -> IO ()
repl p0 = do
    hSetBuffering stdout NoBuffering
    go $ simp p0
    where
        go p = do
            let evs = [ev | ev <- S.toList $ candidates p, isJust $ tryStep p ev]
            print p
            ev <- if Prelude.null evs then askEv p else askEvByIdx evs p
            go $ simp $ fromJust $ tryStep p ev

-- 1通りのトレースしか起き得ない場合、それを辿る
autoStep :: (Data ev, Eq ev, Ord ev, Read ev, Show ev) => (ev -> IO ()) -> ProcExp ev -> IO (ProcExp ev)
autoStep eventHandler p0 = go $ simp p0
    where
        go p | skipExists p = return p
             | otherwise    = do
               let evs = [ev | ev <- S.toList $ candidates p, isJust $ tryStep p ev]
               case evs of
                   [ev] -> do
                       eventHandler ev
                       go $ simp $ fromJust $ tryStep p ev
                   _    -> return p


