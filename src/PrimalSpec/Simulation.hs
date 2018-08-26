module PrimalSpec.Simulation
( repl
--, autoStep
, batchStep
) where

import PrimalSpec.ProcExp
import Control.Monad.State
import Data.Monoid ((<>))
import Data.Maybe (isJust, fromJust)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import Data.Set as S


askEv :: (Data ev, Eq ev, Read ev, Show ev) => ProcExp s ev -> IO ev
askEv p = askStdIn "input acceptable event" (isJust . tryStep p)

askEvByIdx :: (Data ev, Eq ev, Read ev, Show ev) => [ev] -> ProcExp s ev -> IO (Maybe ev)
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

printProcExp :: (Data ev, Show ev) => ProcExp s ev -> IO ()
printProcExp p = do
    putStrLn "---------------"
    print p
    putStrLn "---------------"


repl :: (Data ev, Eq ev, Ord ev, Read ev, Show ev, Show s) => s -> ProcExp s ev -> IO ()
repl s0 p0 = do
    hSetBuffering stdout NoBuffering
    go s0 p0
    where
        go s p = do
            let p' = load s p
            let evs = [ev | ev <- S.toList $ candidates p', isJust $ tryStep p' ev]
            print s
            printProcExp p'
            mev <- askEvByIdx evs p'
            case mev of
                Just ev -> do
                    let m = fromJust $ tryStep p' ev
                    let (p'', s') = runState m s
                    go s' $ simp p''
                Nothing -> return ()

batchStep :: (Data ev, Eq ev, Ord ev, Read ev, Show ev, Show s) => s -> ProcExp s ev -> [ev] -> IO (ProcExp s ev)
batchStep s0 p0 evs0 = do
    printProcExp p0
    go s0 p0 evs0
    where
        go _ p [] = return p
        go _ Stop _ = error "cannot trans"
        go s p (ev:evs)= do
               let p' = load s p
               putStrLn $ "* " <> show ev
               let m = fromJust $ tryStep p' ev
               let (p'', s') = runState m s
               printProcExp p''
               print s'
               putStrLn "---------------"
               go s' (simp p'') evs


--autoStep :: (Data ev, Eq ev, Ord ev, Read ev, Show ev, Show s) => s -> ProcExp s ev -> IO (ProcExp s ev)
--autoStep s0 p0 = do
--    printProcExp p0
--    go s0 p0
--    where
--        go _ Stop = return Stop
--        go s p = do
--               let p' = load s p
--               let evs = [ev | ev <- S.toList $ candidates p', isJust $ tryStep p' ev]
--               case evs of
--                   [ev] -> do
--                       putStrLn $ "* " <> show ev
--                       let m = fromJust $ tryStep p' ev
--                       let (p'', s') = runState m s
--                       printProcExp p''
--                       go s' $ simp p''
--                   _    -> return p
--

