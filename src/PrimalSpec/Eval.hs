{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module PrimalSpec.Eval
  ( evalExpr
  , trans
  , predefinedValue
  ) where

import PrimalSpec.Type
import PrimalSpec.Util
import Data.Maybe (isJust, fromJust)
import Control.Lens hiding (op)
import Control.Applicative ((<|>))
import Control.Monad (when, foldM, unless)
import Control.Monad.Trans.State.Lazy (StateT, evalStateT)
import Control.Monad.Except(throwError, catchError)
import GHC.Stack (HasCallStack)

--import Debug.Trace

data Eval = Eval {
    _counter  :: Int
  , _vctx     :: VCtx
  , _global   :: Maybe Value
}

makeLenses ''Eval

type EvalM a = StateT Eval (Either String) a

runEvalM :: VCtx -> EvalM Value -> Either String Value
runEvalM ctx m = evalStateT m $ Eval 0 ctx Nothing

match :: Loc -> (Pattern, Value) -> EvalM VCtx
match loc (PInt _ n, VInt m)
    | m == n           = return []
    | otherwise        = throwError $ loc ++ "not match(int)"
match loc (PBool _ b, VBool b')
    | b == b'          = return []
    | otherwise        = throwError $ loc ++ "not match(bool)"
match _ (PWildcard _, _) = return []
match _ (PVar _ s, v)    = return [(s, v)]
match _ (PParenthesis l p, v)    = match l (p, v)
match _ (PConstr loc c pts, VConstr c' vs) = if c == c'
    then matchPatterns loc pts vs
    else throwError $ c ++ " does not match " ++ c
match loc pv               = throwError $ loc ++ "not match" ++ show pv

matchPatterns :: Loc -> [Pattern] -> [Value] -> EvalM VCtx
matchPatterns loc pts vs = do
    when (length pts /= length vs) $ throwError $ loc ++ "pattern match failure(num of patterns)"
    concat <$> sequence ( match loc <$> zip pts vs)

evalExpr :: VCtx -> Expr -> Either String Value
evalExpr ctx e = runEvalM ctx $ mkEval [] e

-- TODO global var should only in Proc
-- thunk exists only in vctx
lookupVCtx :: VCtx -> String -> Loc -> EvalM Value
lookupVCtx new k loc = do
    cnt <- use counter
    ctx <- use vctx
    counter += 1
    when (cnt > 1000) $ error $ loc ++ "stack over flow"
    v <- lookupCtx (appendElmsCtx new ctx) k $ loc ++ k ++ " is undefined(value)"
    v'' <- case v of
        VThunk e -> do
            v' <- mkEval [] e
            vctx %= updateCtx (k,v')
            return v'
        VThunkProc e -> do
            v' <- mkEval [] e
            return v'
        _        -> return v
    counter -= 1
    return v''


mkEval :: (HasCallStack) => VCtx -> Expr -> EvalM Value
mkEval _ (EInt           _   v )     = return $ VInt v
mkEval _ (EBool          _   v )     = return $ VBool v
mkEval _ (EVar           _ "global" )  = do
    m <- use global
    unless (isJust m) $ error "global should be set"
    return $ fromJust m
mkEval ctx (EVar           loc v )     = lookupVCtx ctx v loc
mkEval ctx (EId            loc v )     = lookupVCtx ctx v loc <|>  return (VConstr v [])
mkEval ctx (EConstr        _   c as)   = do
    vs <- sequence $ mkEval ctx <$> as
    return $ VConstr c vs
mkEval ctx (EParenthesis   _   e )     = mkEval ctx e
mkEval ctx (EAbst          _   args e) = return $ VClosure args ctx e
mkEval ctx (EApply   loc ef eas) =  do
    tf <- mkEval ctx ef
    vas <- sequence $ mkEval ctx <$> eas
    case tf of
        VFun defs -> foldr1 (<|>) [go def vas | def <- defs]
        VClosure pts ctx' e -> do
            new <- matchPatterns loc pts vas
            v <- mkEval (appendElmsCtx new ctx') e
            return v
        _ -> throwError $ loc ++ "applied type is not function"
    where
        go (pts, e) vs = do
            new <- matchPatterns loc pts vs
            v <- mkEval new e
            return v


mkEval ctx (ETimes   _ e1 e2 ) = apInt    (*)      (mkEval ctx e1) (mkEval ctx e2)
mkEval ctx (EDiv     _ e1 e2 ) = apInt    div      (mkEval ctx e1) (mkEval ctx e2)
mkEval ctx (EMod     _ e1 e2 ) = apInt    mod      (mkEval ctx e1) (mkEval ctx e2)
mkEval ctx (EPower   _ e1 e2 ) = apInt    (^)      (mkEval ctx e1) (mkEval ctx e2)
mkEval ctx (EPlus    _ e1 e2 ) = apInt    (+)      (mkEval ctx e1) (mkEval ctx e2)
mkEval ctx (EMinus   _ e1 e2 ) = apInt    (-)      (mkEval ctx e1) (mkEval ctx e2)
mkEval ctx (EAnd     _ e1 e2 ) = apBool   (&&)     (mkEval ctx e1) (mkEval ctx e2)
mkEval ctx (EOr      _ e1 e2 ) = apBool   (||)     (mkEval ctx e1) (mkEval ctx e2)
mkEval ctx (ELT      _ e1 e2 ) = apIntCmp (<)      (mkEval ctx e1) (mkEval ctx e2)
mkEval ctx (EGT      _ e1 e2 ) = apIntCmp (>)      (mkEval ctx e1) (mkEval ctx e2)
mkEval ctx (ELE      _ e1 e2 ) = apIntCmp (<=)     (mkEval ctx e1) (mkEval ctx e2)
mkEval ctx (EGE      _ e1 e2 ) = apIntCmp (>=)     (mkEval ctx e1) (mkEval ctx e2)
mkEval ctx (EEQ      _ e1 e2 ) = apCmp (eqValue)   (mkEval ctx e1) (mkEval ctx e2)
mkEval ctx (ENE      _ e1 e2 ) = apCmp (nqValue)   (mkEval ctx e1) (mkEval ctx e2)
mkEval ctx (EUnMinus _ e1    ) = fmapInt  negate   (mkEval ctx e1)
mkEval ctx (EUnNot   _ e1    ) = fmapBool not      (mkEval ctx e1)
mkEval ctx (EIf      _ e1 e2 e3) = do
    (VBool v1) <- mkEval ctx e1
    mkEval ctx $ if v1 then e2 else e3
mkEval ctx (ELet      loc pt e1 e2) = do
    v <- mkEval ctx e1
    new <- match loc (pt, v)
    v' <- mkEval (appendElmsCtx new ctx) e2
    return v'

mkEval ctx (EFieldAccess l e accs) = do
    v <- mkEval ctx e
    access l v accs

mkEval ctx (EActionUpdate _ e acts) = do
    v <- mkEval ctx e
    foldM f v acts
    where
    f v (Action l accs e') = do
        lns <- concatAccess l accs
        v' <- mkEval ctx e'
        return $ v & lns .~ v'


mkEval ctx (ERefTrace   loc e1 e2) = do
    (VProc v2) <- mkEval ctx e2
    evs <- extractEvents v2
    (VProc v1) <- mkEval ctx e1
    b <- (go evs v1) `catchError` (\e -> do {dlogM EVENT_TRACE e; return False})
    return $ VBool b
    where
        go [] _       = return True
        go ((l,ev):ev1) v = do
            g <- use global
            dlogM EVENT_TRACE $ "--------------"
            dlogM EVENT_TRACE $ "STATE"
            dlogM EVENT_TRACE $ "--------------"
            dlogM EVENT_TRACE $ show g ++ "\n"
            dlogM EVENT_TRACE $ "--------------"
            dlogM EVENT_TRACE $ "PROCESS"
            dlogM EVENT_TRACE $ "--------------"
            dlogM EVENT_TRACE $ show v ++ "\n"
            --dlogM EVENT_TRACE $ "--------------"
            --dlogM EVENT_TRACE $ "CONTEXT"
            --dlogM EVENT_TRACE $ "--------------"
            --dlogM EVENT_TRACE $ showCtx ctx' ++ "\n"
            dlogM EVENT_TRACE $ "--------------"
            dlogM EVENT_TRACE $ "TRY EVENT"
            dlogM EVENT_TRACE $ "--------------"
            dlogM EVENT_TRACE $ l ++ show ev ++ "\n"
            mk <- trans ev v
            case mk of
                Just k -> go ev1 k
                Nothing -> do
                    dlogM EVENT_TRACE $ loc ++ "cannot trans" ++ "\n"
                    throwError ""

mkEval ctx (EPrefix    _ ev e me) = do
    return $ VProc $ VPrefix ctx ev e me
mkEval ctx (EEChoise   _ e1 e2) = do
    (VProc v1) <- mkEval ctx e1
    (VProc v2) <- mkEval ctx e2
    return $ VProc $ VEChoise v1 v2
mkEval ctx (EGuard     l e1 e2 ) = do
    b <- mkEval ctx e1
    case b of
        VBool True -> mkEval ctx e2
        VBool False -> do
            dlogM EVENT_TRACE $ "=============="
            dlogM EVENT_TRACE $ "GUARD FAILURE"
            dlogM EVENT_TRACE $ "=============="
            dlogM EVENT_TRACE $ l
            return $ VProc VStop
        _ -> error "bug"
mkEval ctx (ESequence  _ e1 e2 ) = do
    VProc v1 <- mkEval ctx e1
    return $ VProc $ VSequence v1 (ctx, e2)
mkEval ctx (EInterrupt _ e1 e2 ) = do
    VProc v1 <- mkEval ctx e1
    VProc v2 <- mkEval ctx e2
    return $ VProc $ VInterrupt v1 v2
mkEval ctx (EProcRef loc s [] ) = lookupVCtx ctx s loc
mkEval ctx (EProcRef loc s es ) = do
    (VProcFun defs) <- lookupVCtx ctx s loc
    vas <- sequence $ mkEval ctx <$> es
    (ctx', e') <- foldr1 (<|>) [go def vas | def <- defs]
    mkEval ctx' e'
    where
        go (pts, e) vs = do
            new <- matchPatterns loc pts vs
            return (new, e)

apCmp :: (t1 -> t2 -> Bool) -> EvalM t1 -> EvalM t2 -> EvalM Value
apCmp op m1 m2 = do
    v1 <- m1
    v2 <- m2
    return $ VBool $ op v1 v2

apInt :: (Int -> Int -> Int) -> EvalM Value -> EvalM Value -> EvalM Value
apInt op m1 m2 = do
    (VInt v1) <- m1
    (VInt v2) <- m2
    return $ VInt $ op v1 v2

apIntCmp :: (Int -> Int -> Bool) -> EvalM Value -> EvalM Value -> EvalM Value
apIntCmp op m1 m2 = do
    (VInt v1) <- m1
    (VInt v2) <- m2
    return $ VBool $ op v1 v2

apBool :: (Bool -> Bool -> Bool) -> EvalM Value -> EvalM Value -> EvalM Value
apBool op m1 m2 = do
    (VBool v1) <- m1
    (VBool v2) <- m2
    return $ VBool $ op v1 v2

fmapInt  :: (Int -> Int) -> EvalM Value -> EvalM Value
fmapInt op m1 = do
    (VInt v1) <- m1
    return $ VInt $ op v1

fmapBool :: (Bool -> Bool) -> EvalM Value -> EvalM Value
fmapBool op m1 = do
    (VBool v1) <- m1
    return $ VBool $ op v1

access :: Loc -> Value -> [Accessor] -> EvalM Value
access _ v [] = return v
access l v accs = do
    lns <- concatAccess l accs
    return $ v ^?! lns

concatAccess :: Applicative f => Loc -> [Accessor] -> EvalM ((Value -> f Value) -> Value -> f Value)
concatAccess _ [] = return id
concatAccess l (Accessor _ aname : accs) = do
    (VAccessor n) <- lookupVCtx [] aname l
    lns <- concatAccess l accs
    return $ (_VConstr . _2 . ix n) . lns

matchEvent :: VCtx -> (EEvent, VEvent) -> EvalM (Maybe VCtx)
matchEvent ctx (EEvent _ c pls, VEvent c' vs) = do
    if c == c'
        then fmap concat . sequence <$> sequence [matchPayload (pl, v) | (pl,v) <- zip pls vs]
        else return Nothing
    where
    matchPayload (PLExp loc e, v) = do
        v' <- mkEval ctx e
        if eqValue v' v then return $ Just [] else do
                dlogM EVENT_TRACE $ loc ++ c ++ " payload matching fail! "
                dlogM EVENT_TRACE $ show v
                dlogM EVENT_TRACE $ show v'
                return Nothing
    matchPayload (PLPat loc p, v) = (Just <$> match loc (p, v)) `catchError` (\e -> do
                dlogM EVENT_TRACE $ e
                return Nothing
                )
    matchPayload (PLElm loc s1 s2 s3 _ ePred, v) = do
        when (s1 /= s2 || s2 /= s3) $ throwError $ loc ++ "this syntax is not implemented"
        (do
            (VBool b) <- mkEval (appendElmsCtx [(s1, v)] ctx) ePred
            return $ if b then Just [(s1, v)] else Nothing) `catchError` (\e -> do
                dlogM EVENT_TRACE $ e
                return Nothing
                )

trans :: (HasCallStack) => VEvent -> VProc -> EvalM (Maybe VProc)
trans = trans'

trans' :: VEvent -> VProc -> EvalM (Maybe VProc)
trans' ev (VPrefix  ctx eev e me) = do
    mctx <- matchEvent ctx (eev, ev)
    case mctx of
        Nothing  -> return Nothing
        Just new -> do
            let ctx' = appendElmsCtx new ctx
            case me of
                Just e' -> do
                    g <- mkEval ctx' e'
                    dlogM EVENT_TRACE $ "=============="
                    dlogM EVENT_TRACE $ "CHANGE Global"
                    dlogM EVENT_TRACE $ "==============\n"
                    global .= Just g
                _ -> return ()
            (VProc v) <- mkEval ctx' e
            --dlogM TRACE_CTX $ showCtx $ v ^? _VPrefix . _1
            return $ Just v

trans' ev (VEChoise  v1  v2) = do
    g0 <- use global
    v1'  <- trans ev v1
    g1 <- use global
    global .= g0
    v2' <- trans ev v2
    g2 <- use global
    global .= g0
    if
       | isJust v1' && isJust v2' -> throwError "undeterministic choise"
       | isJust v1'               -> do { global .= g1; return v1'}
       | isJust v2'               -> do { global .= g2; return v2'}
       | otherwise                -> return Nothing

trans' ev (VSequence v1 (ctx, e))      = do
    case v1 of
        VSkip -> do
            (VProc v2) <- mkEval ctx e
            trans' ev v2
        _ -> do
            mv1' <- trans' ev v1
            return $ mv1' >>= \v -> return $ VSequence v (ctx, e)

trans' ev (VInterrupt v1 v2)      = do
    g0 <- use global
    v1'  <- trans ev v1
    g1 <- use global
    global .= g0
    v2' <- trans ev v2
    g2 <- use global
    global .= g0
    if
       | isJust v1' && isJust v2' -> throwError $ show ev ++ "undeterministic choise"
       | isJust v1'               -> do { global .= g1; return $ return $ VInterrupt (fromJust v1') v2}
       | isJust v2'               -> do { global .= g2; return v2'}
       | otherwise                -> return Nothing

trans' _ VStop = return Nothing
trans' _ VSkip = return Nothing

evalPayload :: VCtx -> Payload -> EvalM Value
evalPayload ctx (PLExp _ e) = mkEval ctx e
evalPayload _ p = throwError $ showLoc p ++ "caanot calc value"

evalEvent :: VCtx -> EEvent -> EvalM VEvent
evalEvent ctx (EEvent _ c pls) = do
    vs <- sequence $ evalPayload ctx <$> pls
    return $ VEvent c vs

extractEvents :: VProc -> EvalM [(Loc, VEvent)]
extractEvents VSkip = return []
extractEvents (VPrefix ctx ee e _) = do
    ev <- evalEvent ctx ee
    VProc e' <- mkEval ctx e
    evs <- extractEvents e'
    return $ (showLoc ee, ev):evs
extractEvents (VSequence (v1) (c, e2)) = do
    es1 <- extractEvents v1
    VProc p2 <- mkEval c e2
    es2 <- extractEvents p2
    return $ es1 ++ es2
extractEvents _ = throwError "invalid proc expr"

predefinedValue :: VCtx
predefinedValue = [("SKIP", VProc VSkip), ("STOP", VProc VStop)]

