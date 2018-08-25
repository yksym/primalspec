module PrimalSpec.ProcExp
( ProcExp(Stop, Skip)
, (-->)
, (?->)
, (&->)
, (&!->)
, (*?*)
, (|=|)
-- , (PrimalSpec.ProcExp.<|>)
, (<||>)
, (>>>)
, candidates
, simp
, tryStep
, isTerminable
, module PrimalSpec.Util
, Data
) where

import PrimalSpec.ConstrUtil
import PrimalSpec.Util
import Control.Applicative as A
import Data.Data (Data)
import Data.Monoid ((<>))
import Data.Set as S
import Data.Maybe (isJust, isNothing)
import Text.Printf (printf)


data ProcExp ev
    = Stop
    | Skip
    | Prefix ev (ProcExp ev)
    | forall a. Recv (a -> ev) (a -> ProcExp ev)
    | ExternalChoise (ProcExp ev) (ProcExp ev)
    -- | Interrupt (ProcExp ev) (ProcExp ev)
    | Sequential (ProcExp ev) (ProcExp ev)
    | Parallel (ProcExp ev) (ProcExp ev)
    | forall a. (Show a) => DebugShow a (ProcExp ev)

instance forall ev. (Show ev, Data ev) => Show (ProcExp ev) where
    show = go 0 "" where
        go depth pre Skip                   = indent depth <> pre <> "Skip" <> endl
        go depth pre Stop                   = indent depth <> pre <> "Stop" <> endl
        go depth pre (Prefix ev _)          = indent depth <> pre <> show ev <> " -> ..." <> endl
        go depth pre (Recv evc _)           = indent depth <> pre <> constrNameOf evc <> "?x -> ..." <> endl
        go depth pre (ExternalChoise p1 p2) = indent depth <> pre <> "|=|" <> endl <> mconcat [go (depth+1) "" p | p <- [p1, p2]]
        --go depth pre (Interrupt p1 p2)      = indent depth <> pre <> "<|>" <> endl <> go (depth+1) "" p1 <> go (depth+1) "" p2
        go depth pre (Sequential p1 p2)     = indent depth <> pre <> " ; " <> endl <> go (depth+1) "" p1 <> go (depth+1) "" p2
        go depth pre (Parallel p1 p2)       = indent depth <> pre <> "<||>" <> endl <> go (depth+1) "" p1 <> go (depth+1) "" p2
        go depth pre (DebugShow s p)        = indent depth <> pre <> s' <> endl <> go depth pre p
            where
            s' = printf "--dbg: %s\n" $ show s
        indent n = replicate (2*n) ' '
        endl = "\n"


(-->) :: ev -> ProcExp ev -> ProcExp ev
(-->) = Prefix

(?->) :: (a -> ev) -> (a -> ProcExp ev) -> ProcExp ev
(?->) = Recv

-- guard
(&->) :: Bool -> ProcExp ev -> ProcExp ev
b &-> p = if b then p else Stop

(*?*) :: (Show a) => a -> ProcExp ev -> ProcExp ev
(*?*) =  DebugShow

(&!->) :: Bool -> ProcExp ev -> ProcExp ev
b &!-> p =  if b then p else "assert failed" *?* Stop

infixr 4  -->, ?->, &->, &!->, *?*

(|=|) :: ProcExp ev -> ProcExp ev -> ProcExp ev
(|=|) = ExternalChoise

--(<|>) :: ProcExp ev -> ProcExp ev -> ProcExp ev
--(<|>) = Interrupt

(<||>) :: ProcExp ev -> ProcExp ev -> ProcExp ev
(<||>) = Parallel

(>>>) :: ProcExp ev -> ProcExp ev -> ProcExp ev
(>>>) = Sequential

infixl 3 >>>,  |=|, <||> -- <|>, 

simp :: (Show ev, Data ev) => ProcExp ev -> ProcExp ev
simp (Sequential p1 p2)             = case simp p1 of
    Stop   -> Stop
    p1'    -> Sequential p1' p2
simp (ExternalChoise p1 Stop)       = simp p1
simp (ExternalChoise Stop p2)       = simp p2
simp (ExternalChoise p1 p2)         = ExternalChoise (simp p1) (simp p2)
simp (Parallel Stop _)              = Stop
simp (Parallel _ Stop )             = Stop
simp (Parallel p1 p2)               = Parallel (simp p1) (simp p2)
simp (DebugShow s p)                = DebugShow s $ simp p
simp p = p


isTerminable :: (Data ev, Eq ev, Show ev) => ProcExp ev -> Bool
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
isTerminable (Parallel p1 p2)
    | p1' && p2' = True
    | otherwise  = False
    where
        p1' = isTerminable p1
        p2' = isTerminable p2
isTerminable (DebugShow _ p) =  isTerminable p


tryStep :: (Data ev, Eq ev, Show ev) => ProcExp ev -> ev -> Maybe (ProcExp ev)
tryStep Skip _ = Nothing
tryStep Stop _ = Nothing
tryStep (Prefix ev1 p) ev2 | ev1 == ev2 = Just p
                           | otherwise  = Nothing
tryStep (Recv ev1c p) ev2 = p <$> argOf ev2 ev1c
tryStep (ExternalChoise p1 p2) ev
    | isJust p1' && isJust p2' = error "ExternalChoise for same event!!"
    | otherwise  = p1' A.<|> p2'
    where
        p1' = tryStep p1 ev
        p2' = tryStep p2 ev
tryStep (Sequential p1 p2) ev
    | isJust p1' && not b1 || isNothing p2' = Sequential <$> p1' <*> Just p2
    | b1  && isNothing p1' && isJust p2'    = p2'
    | otherwise = Nothing
    where
        p1' = tryStep p1 ev
        p2' = tryStep p2 ev
        b1 = isTerminable p1
--tryStep (Interrupt p1 p2) ev
--    | isJust p1' && isJust p2' = error "Interrupt for same event!!"
--    | isJust p2' = p2'
--    | otherwise  = Interrupt <$> p1' <*> return p2
--    where
--        p1' = tryStep p1 ev
--        p2' = tryStep p2 ev
tryStep (Parallel p1 p2) ev = Parallel <$> p1' <*> p2'
    where
        p1'  = tryStep p1 ev
        p2'  = tryStep p2 ev
tryStep (DebugShow _ p) ev =  tryStep p ev


candidates :: (Ord ev) => ProcExp ev -> S.Set ev
candidates (Prefix ev _) = S.singleton ev
candidates (ExternalChoise p1 p2) = candidates p1 `S.union` candidates p2
--candidates (Interrupt p1 p2) = candidates p1 `S.union` candidates p2
--candidates (Parallel p1 p2) = candidates p1 `S.intersection` candidates p2
candidates (Parallel p1 p2) = candidates p1 `S.union` candidates p2
candidates _ = S.empty

