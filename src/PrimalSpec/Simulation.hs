module PrimalSpec.Simulation
( repl
, autoStep
) where

import PrimalSpec.ProcExp
import Data.Monoid ((<>))
import Data.Maybe (isJust, fromJust)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import Data.Set as S


askEv :: (Data ev, Eq ev, Read ev, Show ev) => ProcExp ev -> IO ev
askEv p = askStdIn "input acceptable event" (isJust . tryStep p)

askEvByIdx :: (Data ev, Eq ev, Read ev, Show ev) => [ev] -> ProcExp ev -> IO (Maybe ev)
askEvByIdx evs p = do
    printEvents
    idx <- askStdIn ("input " <> cands <> " 0 : perform another event. other num : temination") (<= length evs)
    if
        | idx > 0 && (idx - 1) < length evs -> return $ Just $ evs !! (idx-1)
        | idx == 0 -> Just <$> askEv p
        | isTerminable p -> return Nothing
        | otherwise -> print "termination failed" >> askEvByIdx evs p
    where
        printIndeces = putStr <$> [show n <> " : " | n <- [1..(length evs)]]
        printEvs     = print <$> evs
        printEvents  = sequence_ $ interleave printIndeces printEvs
        cands = if Prelude.null evs then "" else "[1-" <> show (length evs) <> "]."

printProcExp :: (Data ev, Show ev) => ProcExp ev -> IO ()
printProcExp p = do
    putStrLn "---------------"
    print p
    putStrLn "---------------"


repl :: (Data ev, Eq ev, Ord ev, Read ev, Show ev) => ProcExp ev -> IO ()
repl p0 = do
    hSetBuffering stdout NoBuffering
    go $ simp p0
    where
        go p = do
            let evs = [ev | ev <- S.toList $ candidates p, isJust $ tryStep p ev]
            printProcExp p
            mev <- askEvByIdx evs p
            case mev of
                Just ev -> go $ simp $ fromJust $ tryStep p ev
                Nothing -> return ()

autoStep :: (Data ev, Eq ev, Ord ev, Read ev, Show ev) => ProcExp ev -> IO (ProcExp ev)
autoStep p0 = do
    printProcExp p0
    go $ simp p0
    where
        go Stop = return Stop
        go p = do
               let evs = [ev | ev <- S.toList $ candidates p, isJust $ tryStep p ev]
               case evs of
                   [ev] -> do
                       putStrLn $ "* " <> show ev
                       let p' = simp $ fromJust $ tryStep p ev
                       printProcExp p'
                       go p'
                   _    -> return p


