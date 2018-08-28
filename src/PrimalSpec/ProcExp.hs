module PrimalSpec.ProcExp
( ProcExp(Stop, Skip, Load)
, (-->)
, (%->)
, (?->)
, (&->)
, (|=|)
, (PrimalSpec.ProcExp.<|>)
, (<@>)
, (<||>)
, (|||)
, (>>>)
, candidates
, simp
, tryStep
, isTerminable
, module PrimalSpec.Util
, Data
, nop
, load
) where

import PrimalSpec.ConstrUtil
import PrimalSpec.Util
import Control.Applicative as A
import Control.Monad.State
import Data.Data (Data)
import Data.Monoid ((<>))
import Data.Set as S
import Data.Maybe (isJust, isNothing)
--import Text.Printf (printf)


data Store s a = Store (State s ()) a

data ProcExp s ev
    = Stop
    | Skip
    | Prefix ev (Store s (ProcExp s ev))
    | forall a. Recv (a -> ev) (a -> Store s (ProcExp s ev))
    | ExternalChoise (ProcExp s ev) (ProcExp s ev)
    | Interrupt (ProcExp s ev) (ProcExp s ev)
    | RetInterrupt (ProcExp s ev) (ProcExp s ev)
    | Sequential (ProcExp s ev) (ProcExp s ev)
    | Parallel (ProcExp s ev) (ProcExp s ev)
    | Interleave (ProcExp s ev) (ProcExp s ev)
    | Load (s -> ProcExp s ev)

instance forall ev s. (Show ev, Data ev) => Show (ProcExp s ev) where
    show = go [] where
        go bs Skip                   = indent bs <> "Skip" <> endl
        go bs Stop                   = indent bs <> "Stop" <> endl
        go bs (Prefix ev _)          = indent bs <> show ev <> " -> ..." <> endl
        go bs (Recv evc _)           = indent bs <> constrNameOf evc <> "?x -> ..." <> endl
        go bs (ExternalChoise p1 p2) = indent bs <> "|=|"  <> endl <> go (bs++[True]) p1 <> go (bs++[False]) p2
        go bs (Interrupt p1 p2)      = indent bs <> "<|>"  <> endl <> go (bs++[True]) p1 <> go (bs++[False]) p2
        go bs (RetInterrupt p1 p2)   = indent bs <> "<@>"  <> endl <> go (bs++[True]) p1 <> go (bs++[False]) p2
        go bs (Sequential p1 p2)     = indent bs <> " ; "  <> endl <> go (bs++[True]) p1 <> go (bs++[False]) p2
        go bs (Parallel p1 p2)       = indent bs <> "<||>" <> endl <> go (bs++[True]) p1 <> go (bs++[False]) p2
        go bs (Interleave p1 p2)     = indent bs <> "|||"  <> endl <> go (bs++[True]) p1 <> go (bs++[False]) p2
        go bs (Load _)               = indent bs <> "???"
        indent [] = "--* "
        indent [True] = "  |----* "
        indent [False] = "  `----* "
        indent (b:bs) = (if b then "  |  " else "     ") <> indent bs
        endl = "\n"

(-->) :: ev -> Store s (ProcExp s ev) -> ProcExp s ev
(-->) = Prefix

(?->) :: (a -> ev) -> (a -> Store s (ProcExp s ev)) -> ProcExp s ev
(?->) = Recv

-- guard
(&->) :: Bool -> ProcExp s ev -> ProcExp s ev
b &-> p = if b then p else Stop

-- update
(%->) :: State s () -> ProcExp s ev -> Store s (ProcExp s ev)
(%->) = Store

nop :: State s ()
nop = return ()

infixr 4  -->, ?->, &->, %-> -- *?*, 

(|=|) :: ProcExp s ev -> ProcExp s ev -> ProcExp s ev
(|=|) = ExternalChoise

(<|>) :: ProcExp s ev -> ProcExp s ev -> ProcExp s ev
(<|>) = Interrupt

(<@>) :: ProcExp s ev -> ProcExp s ev -> ProcExp s ev
(<@>) = RetInterrupt

(<||>) :: ProcExp s ev -> ProcExp s ev -> ProcExp s ev
(<||>) = Parallel

(|||) :: ProcExp s ev -> ProcExp s ev -> ProcExp s ev
(|||) = Interleave

(>>>) :: ProcExp s ev -> ProcExp s ev -> ProcExp s ev
(>>>) = Sequential

infixl 3 >>>,  |=|, <|>, <@>, <||>, |||

simp :: (Show ev, Data ev) => ProcExp s ev -> ProcExp s ev
simp (Sequential p1 p2)             = case simp p1 of
    Stop   -> Stop
    p1'    -> Sequential p1' p2
simp (ExternalChoise p1 Stop)       = simp p1
simp (ExternalChoise Stop p2)       = simp p2
simp (ExternalChoise p1 p2)         = ExternalChoise (simp p1) (simp p2)
simp (Interrupt p1 p2)              = case (simp p1, simp p2) of
    (Stop, Stop)   -> Stop
    (Stop, p2')    -> p2'
    (p1', Stop)    -> p1'
    _              -> Interrupt p1 p2
simp (RetInterrupt p1 p2)           = case (simp p1, simp p2) of
    (Stop, Stop)   -> Stop
    (p1', Stop)    -> p1'
    _              -> RetInterrupt p1 p2
simp (Parallel Stop _)              = Stop
simp (Parallel _ Stop )             = Stop
simp (Parallel p1 p2)               = Parallel (simp p1) (simp p2)
simp (Interleave Stop p2)           = simp p2
simp (Interleave p1 Stop)           = simp p1
simp (Interleave p1 p2)             = Interleave (simp p1) (simp p2)
simp p = p


isTerminable :: (Data ev, Eq ev, Show ev) => ProcExp s ev -> Bool
isTerminable Skip = True
isTerminable Stop = False
isTerminable (Prefix _ _) = False
isTerminable (Recv _ _)   = False
isTerminable (ExternalChoise p1 p2)
    | p1' && p2' = error "ExternalChoise for same event!!"
    | otherwise  = p1' || p2'
    where
        p1' = isTerminable p1
        p2' = isTerminable p2
isTerminable (Sequential p1 p2)
    | p1' && p2' = True
    | otherwise  = False
    where
        p1' = isTerminable p1
        p2' = isTerminable p2
isTerminable (Interrupt p1 p2) = p1' || p2'
    where
        p1' = isTerminable p1
        p2' = isTerminable p2
isTerminable (RetInterrupt p1 _) = p1'
    where
        p1' = isTerminable p1
isTerminable (Parallel p1 p2)
    | p1' && p2' = True
    | otherwise  = False
    where
        p1' = isTerminable p1
        p2' = isTerminable p2
isTerminable (Interleave p1 p2)
    | p1' && p2' = True
    | otherwise  = False
    where
        p1' = isTerminable p1
        p2' = isTerminable p2
isTerminable (Load _) = error "Load is detected in check termination"


tryStep :: (Data ev, Eq ev, Show ev) => s -> ProcExp s ev -> ev -> Maybe (State s (ProcExp s ev))
tryStep _ Skip _ = Nothing
tryStep _ Stop _ = Nothing
tryStep _ (Prefix ev1 (Store m p)) ev2 | ev1 == ev2 = Just $ m >> return p
                                     | otherwise = Nothing
tryStep _ (Recv ev1c p) ev2 = case p <$> argOf ev2 ev1c of
    Just (Store m p') -> Just $ m >> return p'
    Nothing -> Nothing
tryStep s (ExternalChoise p1 p2) ev
    | isJust p1' && isJust p2' = error "ExternalChoise for same event!!" -- how does monad transfer treat this without s ??
    | otherwise  = p1' A.<|> p2'
    where
        p1' = tryStep s p1 ev
        p2' = tryStep s p2 ev
tryStep s (Interrupt p1 p2) ev
    | isJust p1' && isJust p2' = error "Interrupt for same event!!"
    | isJust p2' = p2'
    | otherwise  = do
        p1'' <- p1'
        return $ Interrupt <$> p1'' <*> return p2
    where
        p1' = tryStep s p1 ev
        p2' = tryStep s p2 ev
tryStep s (RetInterrupt p1 p2) ev
    | isJust p1' && isJust p2' = error "RetInterrupt for same event!!"
    | isJust p2' = do
        p2'' <- p2'
        return $ Sequential <$> p2'' <*> return (RetInterrupt p1 p2)
    | otherwise  = do
        p1'' <- p1'
        return $ RetInterrupt <$> p1'' <*> return p2
    where
        p1' = tryStep s p1 ev
        p2' = tryStep s p2 ev
tryStep s (Sequential p1 p2) ev
    | isJust p1' && not b1 || isNothing p2' = do
        p1'' <- p1'
        return $ Sequential <$> p1'' <*> return p2
    | b1  && isNothing p1' && isJust p2'    = p2'
    | otherwise = Nothing
    where
        p1' = tryStep s p1 ev
        p2' = tryStep s p2 ev
        b1 = isTerminable p1
tryStep s (Parallel p1 p2) ev = do
        p1'' <- p1'
        p2'' <- p2'
        return $ Parallel <$> p1'' <*> p2''
    where
        p1'  = tryStep s p1 ev
        p2'  = tryStep s p2 ev
tryStep s (Interleave p1 p2) ev
    | isJust p1' && isJust p2' = error "RetInterrupt for same event!!"
    | isJust p2' = do
        p2'' <- p2'
        return $ Interleave <$> return p1 <*> p2''
    | otherwise  = do
        p1'' <- p1'
        return $ Interleave <$> p1'' <*> return p2
    where
        p1'  = tryStep s p1 ev
        p2'  = tryStep s p2 ev
tryStep s (Load p) ev = tryStep s (p s) ev


candidates :: (Ord ev) => s -> ProcExp s ev -> S.Set ev
candidates _ (Prefix ev _) = S.singleton ev
candidates s (ExternalChoise p1 p2) = candidates s p1 `S.union` candidates s p2
candidates s (Sequential p1 p2) = candidates s p1 `S.union` candidates s p2
candidates s (Interrupt p1 p2) = candidates s p1 `S.union` candidates s p2
candidates s (RetInterrupt p1 p2) = candidates s p1 `S.union` candidates s p2
candidates s (Parallel p1 p2) = candidates s p1 `S.union` candidates s p2
candidates s (Interleave p1 p2) = candidates s p1 `S.union` candidates s p2
candidates s (Load p) = candidates s (p s)
candidates _ _ = S.empty


load :: s -> ProcExp s ev -> ProcExp s ev
load s (Load f) = load s $ f s
load s (ExternalChoise p1 p2) = ExternalChoise (load s p1) (load s p2)
load s (Sequential p1 p2)     = Sequential (load s p1) (load s p2)
load s (Interrupt p1 p2)      = Interrupt (load s p1) (load s p2)
load s (RetInterrupt p1 p2)   = RetInterrupt (load s p1) (load s p2)
load s (Parallel p1 p2)       = Parallel (load s p1) (load s p2)
load s (Interleave p1 p2)     = Interleave (load s p1) (load s p2)
load _ p = p

