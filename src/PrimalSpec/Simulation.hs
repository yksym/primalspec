module PrimalSpec.Simulation
( repl
--, autoStep
, batchStep
) where

import PrimalSpec.ProcExp
import Control.Monad.State
import Data.Monoid ((<>))
import Data.Maybe (isJust, fromJust, fromMaybe)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import Data.Set as S


askEv :: (Data ev, Eq ev, Read ev, Show ev) => s -> ProcExp s ev -> IO ev
askEv s p = askStdIn "input acceptable event" (isJust . tryStep s p)

askEvByIdx :: (Data ev, Eq ev, Read ev, Show ev) => [ev] -> s -> ProcExp s ev -> IO (Maybe ev)
askEvByIdx evs s p = do
    printEvents
    idx <- askStdIn ("input " <> cands <> " 0 : perform another event. other num : temination") (<= length evs)
    if
        | idx > 0 && (idx - 1) < length evs -> return $ Just $ evs !! (idx-1)
        | idx == 0 -> Just <$> askEv s p
        | isTerminable p -> return Nothing
        | otherwise -> print "termination failed" >> askEvByIdx evs s p
    where
        printIndeces = putStr <$> [show n <> " : " | n <- [1..(length evs)]]
        printEvs     = print <$> evs
        printEvents  = sequence_ $ interleave printIndeces printEvs
        cands = if Prelude.null evs then "" else "[1-" <> show (length evs) <> "]."

printProcExp :: (Data ev, Show ev, Show s) => s -> ProcExp s ev -> IO ()
printProcExp s p = do
    putStrLn "---------------"
    print s
    putStrLn "---------------"
    print $ load s p
    putStrLn "---------------"


repl :: (Data ev, Eq ev, Ord ev, Read ev, Show ev, Show s) => s -> ProcExp s ev -> IO ()
repl s0 p0 = do
    hSetBuffering stdout NoBuffering
    go s0 p0
    where
        go s p = do
            let evs = [ev | ev <- S.toList $ candidates s p, isJust $ tryStep s p ev]
            printProcExp s p
            mev <- askEvByIdx evs s p
            case mev of
                Just ev -> do
                    let m = fromJust $ tryStep s p ev
                    let (p', s') = runState m s
                    go s' $ simp p'
                Nothing -> return ()

batchStep :: (Data ev, Eq ev, Ord ev, Read ev, Show ev, Show s) => s -> ProcExp s ev -> [ev] -> IO (ProcExp s ev)
batchStep s0 p0 evs0 = do
    printProcExp s0 p0
    go s0 p0 evs0
    where
        go _ p [] = return p
        go _ Stop _ = error "cannot trans"
        go s p (ev:evs)= do
               putStrLn $ "* " <> show ev
               let m = fromMaybe (error $ "cannot trans " <> show ev) $ tryStep s p ev
               let (p', s') = runState m s
               printProcExp s' p'
               go s' (simp p') evs

