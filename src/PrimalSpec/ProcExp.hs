{-# LANGUAGE GADTs, ExistentialQuantification, MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module PrimalSpec.ProcExp
(module PrimalSpec.ProcExp)
where

import PrimalSpec.Util
import PrimalSpec.ConstrUtil
import PrimalSpec.NonDeterministicCounter (readNDC, UseNDC)
import Text.Printf (printf)
import Control.Applicative as A
import Data.Data (Data)
import Data.Monoid ((<>))
import Data.Maybe (isJust, fromJust)
import Data.Set as S
import Data.List (transpose)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import Debug.Trace (trace)


data ProcExp ev
    = Stop
    | Skip
    | Prefix ev (ProcExp ev)
    | forall a. Recv (a -> ev) (a -> ProcExp ev)
    | ExternalChoise (ProcExp ev) (ProcExp ev)
    | InternalChoise (ProcExp ev) (ProcExp ev)
    | Interrupt (ProcExp ev) (ProcExp ev)
    | Sequential (ProcExp ev) (ProcExp ev)
    | AlphabetParallel (ev -> Bool) (ev -> Bool) (ProcExp ev) (ProcExp ev)
    -- | InterfaceParallel (ev -> Bool) (ProcExp ev) (ProcExp ev)
    -- | InterruptReturn (ProcExp ev) (ProcExp ev) -- for CPU interrupt handler
    | DebugShow String (ProcExp ev)
    -- | HintTau Int (ProcExp ev)
    -- | HintSuchThat String (ProcExp ev)

instance forall ev. (Show ev, Data ev) => Show (ProcExp ev) where
    show = go 0 "" where
        go depth pre Skip                   = indent depth <> pre <> "Skip" <> endl
        go depth pre Stop                   = indent depth <> pre <> "Stop" <> endl
        go depth pre (Prefix ev _)          = indent depth <> pre <> show ev <> " -> ..." <> endl
        go depth pre (Recv evc _)           = indent depth <> pre <> constrNameOf evc <> "?x -> ..." <> endl
        go depth pre (ExternalChoise p1 p2) = indent depth <> pre <> "|=|" <> endl <> mconcat [go (depth+1) "" p | p <- [p1, p2]]
        go depth pre (InternalChoise p1 p2) = indent depth <> pre <> "|~|" <> endl <> mconcat [go (depth+1) "" p | p <- [p1, p2]]
        go depth pre (Interrupt p1 p2)      = indent depth <> pre <> "<|>" <> endl <> go (depth+1) "" p1 <> go (depth+1) "" p2
        go depth pre (Sequential p1 p2)     = indent depth <> pre <> " ; " <> endl <> go (depth+1) "" p1 <> go (depth+1) "" p2
        go depth pre (AlphabetParallel _ _ p1 p2) = indent depth <> pre <> "<||>" <> endl <> go (depth+1) "" p1 <> go (depth+1) "" p2
        go depth pre (DebugShow _ p)        = go depth pre p
        indent n = replicate (2*n) ' '
        endl = "\n"


(-->) :: ev -> ProcExp ev -> ProcExp ev
(-->) = Prefix

(?->) :: (a -> ev) -> (a -> ProcExp ev) -> ProcExp ev
(?->) = Recv

-- guard
(&->) :: Bool -> ProcExp ev -> ProcExp ev
b &-> p = if b then p else Stop

-- guard
(*!*) :: String -> ProcExp ev -> ProcExp ev
(*!*) =  DebugShow

infixr 4  -->, ?->, &->, *!* -- , *?*, *%*

(|=|) :: ProcExp ev -> ProcExp ev -> ProcExp ev
(|=|) = ExternalChoise

(|~|) :: ProcExp ev -> ProcExp ev -> ProcExp ev
(|~|) = InternalChoise

(<|>) :: ProcExp ev -> ProcExp ev -> ProcExp ev
(<|>) = Interrupt

(<||>) :: ProcExp ev -> ProcExp ev -> ProcExp ev
(<||>) = AlphabetParallel (const True) (const True)

infixl 3  |=|, |~|, <|>, <||>

chooseIC :: (Show a, UseNDC) => a -> a -> a
chooseIC p1 p2 = if readNDC s == 0 then p1 else p2
    where
        s = printf ("InternalChoise is found! input [01].\n" <> " 0: %s\n" <> " 1: %s\n")  (show p1) (show p2)

dbg :: (Show s) => s -> a -> a
dbg s = trace $ printf "\ndbg---------\n%s\n------------\n" $ show s

simp :: (Show ev, Data ev, UseNDC) => ProcExp ev -> ProcExp ev
simp (Interrupt Skip _)             = Skip
simp (Interrupt p1 p2)              = Interrupt (simp p1) (simp p2)
simp (Sequential Skip p)            = simp p -- 今は✓ がないのでここで処理
simp (Sequential p1 p2)             = Sequential (simp p1) p2
simp (ExternalChoise p1 p2)         = ExternalChoise (simp p1) (simp p2)
simp (InternalChoise p1 p2)         = simp $ chooseIC p1 p2  -- 今はtauがinternal choiseしかないのでここで処理
simp (AlphabetParallel pred1 pred2 p1 p2) = AlphabetParallel pred1 pred2 (simp p1) (simp p2)
simp (DebugShow s p) = dbg s $ simp p
simp p = p


tryStep :: (UseNDC, Data ev, Eq ev, Show ev) => ProcExp ev -> ev -> Maybe (ProcExp ev)
tryStep Skip _ = Nothing
tryStep Stop _ = Nothing
tryStep (Prefix ev1 p) ev2 | ev1 == ev2 = Just p
                           | otherwise  = Nothing
tryStep (Recv ev1c p) ev2 = p <$> argOf ev2 ev1c

tryStep (ExternalChoise p1 p2) ev
    | isJust p1' && isJust p2' = error "ExternalChoise for same event!!" -- ここではInternalChoise扱いではなく禁止とする
    | otherwise  = p1' A.<|> p2'
    where
        p1' = tryStep p1 ev
        p2' = tryStep p2 ev
tryStep (Sequential Skip p2) ev = tryStep p2 ev
tryStep (Sequential p1 p2) ev = Sequential <$> tryStep p1 ev <*> Just p2
tryStep (InternalChoise p1 p2) ev = tryStep (chooseIC p1 p2) ev
tryStep (Interrupt p1 p2) ev
    | isJust p2' = p2'
    | otherwise  = do
        p1' <- tryStep p1 ev
        return $ Interrupt p1' p2
    where
        p2' = tryStep p2 ev
tryStep (AlphabetParallel pred1 pred2 p1 p2) ev
    | pred1 ev && pred2 ev = AlphabetParallel pred1 pred2 <$> p1' <*> p2'
    | pred1 ev = AlphabetParallel pred1 pred2 <$> p1' <*> Just p2
    | pred2 ev = AlphabetParallel pred1 pred2 <$> Just p1 <*> p2'
    | otherwise = Nothing
    where
        p1'  = tryStep p1 ev
        p2'  = tryStep p2 ev
tryStep (DebugShow _ p) ev = tryStep p ev


candidates :: (Ord ev) => ProcExp ev -> S.Set ev
candidates (Prefix ev _) = S.singleton ev
candidates (ExternalChoise p1 p2) = candidates p1 `S.union` candidates p2
candidates (InternalChoise p1 p2) = candidates p1 `S.union` candidates p2
candidates (Interrupt p1 p2) = candidates p1 `S.union` candidates p2
candidates (AlphabetParallel pred1 pred2 p1 p2) = S.filter (\a -> pred1 a || pred2 a) $ candidates p1 `S.union` candidates p2
candidates _ = S.empty


interleave :: [a] -> [a] -> [a]
interleave xs ys = concat $ transpose [xs, ys]


askEv :: (UseNDC, Data ev, Eq ev, Read ev, Show ev) => ProcExp ev -> IO ev
askEv p = askStdIn "input acceptable event" (isJust . tryStep p)


askEvByIdx :: (UseNDC, Data ev, Eq ev, Read ev, Show ev) => [ev] -> ProcExp ev -> IO ev
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


repl :: (UseNDC, Data ev, Eq ev, Ord ev, Read ev, Show ev) => ProcExp ev -> IO ()
repl p0 = do
    hSetBuffering stdout NoBuffering
    go $ simp p0
    where
        go p = do
            let evs = [ev | ev <- S.toList $ candidates p, isJust $ tryStep p ev]
            print p
            ev <- if Prelude.null evs then askEv p else askEvByIdx evs p
            go $ simp $ fromJust $ tryStep p ev

