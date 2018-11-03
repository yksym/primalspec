{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module PrimalSpec.Eval
  ( evalExpr
  , trans
  , predefinedValue
  ) where

import PrimalSpec.Type
import Data.Maybe (isJust)
import Control.Lens hiding (op)
import Control.Applicative ((<|>))
import Control.Monad (when, foldM)
import Control.Monad.Trans.State.Lazy (StateT, evalStateT)
import Control.Monad.Except(throwError, catchError)
--import Debug.Trace

data Eval = Eval {
    _counter  :: Int
  , _vctx     :: VCtx
}

makeLenses ''Eval

type EvalM a = StateT Eval (Either String) a

runEvalM :: VCtx -> EvalM Value -> Either String Value
runEvalM ctx m = evalStateT m $ Eval 0 ctx

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
evalExpr ctx e = runEvalM ctx $ mkEval e

lookupVCtx :: String -> Loc -> EvalM Value
lookupVCtx k loc = do
    cnt <- use counter
    ctx <- use vctx
    counter += 1
    when (cnt > 1000) $ throwError $ loc ++ "stack over flow"
    v <- lookupCtx ctx k $ loc ++ k ++ " is undefined(value)"
    v' <- case v of
        VThunk e -> mkEval e
        _        -> return v
    when (v /= v') $ vctx %= updateCtx k v v'
    counter -= 1
    return v'



tryEvalProcExpr :: Expr -> EvalM (VCtx, VProc)
tryEvalProcExpr e = do
    old <- use vctx
    (VProc v) <- mkEval e
    ctx <- use vctx
    vctx .= old
    return (ctx, v)

mkEval :: Expr -> EvalM Value
mkEval (EInt           _   v )     = return $ VInt v
mkEval (EBool          _   v )     = return $ VBool v
mkEval (EVar           loc v )     = lookupVCtx v loc
mkEval (EId            loc v )     = lookupVCtx v loc <|>  return (VConstr v [])
mkEval (EConstr        _   c as)   = do
    vs <- sequence $ mkEval <$> as
    return $ VConstr c vs
mkEval (EParenthesis   _   e )     = mkEval e
mkEval (EAbst          _   args e) = do
    ctx <- use vctx
    return $ VClosure args ctx e
mkEval (EApply   loc ef eas) =  do
    tf <- mkEval ef
    vas <- sequence $ mkEval <$> eas
    case tf of
        VFun defs -> foldr1 (<|>) [go def vas | def <- defs]
        VClosure pts ctx e -> do
            new <- matchPatterns loc pts vas
            vctx %= appendElmsCtx (ctx ++ new)
            v <- mkEval e
            vctx %= deleteElmsCtx (ctx ++ new)
            return v
        _ -> throwError $ loc ++ "applied type is not function"
    where
        go (pts, e) vs = do
            new <- matchPatterns loc pts vs
            vctx %= appendElmsCtx new
            v <- mkEval e
            vctx %= deleteElmsCtx new
            return v


mkEval (ETimes   _ e1 e2 ) = apInt    (*)      (mkEval e1) (mkEval e2)
mkEval (EDiv     _ e1 e2 ) = apInt    div      (mkEval e1) (mkEval e2)
mkEval (EMod     _ e1 e2 ) = apInt    mod      (mkEval e1) (mkEval e2)
mkEval (EPower   _ e1 e2 ) = apInt    (^)      (mkEval e1) (mkEval e2)
mkEval (EPlus    _ e1 e2 ) = apInt    (+)      (mkEval e1) (mkEval e2)
mkEval (EMinus   _ e1 e2 ) = apInt    (-)      (mkEval e1) (mkEval e2)
mkEval (EAnd     _ e1 e2 ) = apBool   (&&)     (mkEval e1) (mkEval e2)
mkEval (EOr      _ e1 e2 ) = apBool   (||)     (mkEval e1) (mkEval e2)
mkEval (ELT      _ e1 e2 ) = apIntCmp (<)      (mkEval e1) (mkEval e2)
mkEval (EGT      _ e1 e2 ) = apIntCmp (>)      (mkEval e1) (mkEval e2)
mkEval (ELE      _ e1 e2 ) = apIntCmp (<=)     (mkEval e1) (mkEval e2)
mkEval (EGE      _ e1 e2 ) = apIntCmp (>=)     (mkEval e1) (mkEval e2)
mkEval (EEQ      _ e1 e2 ) = apIntCmp (==)     (mkEval e1) (mkEval e2)
mkEval (ENE      _ e1 e2 ) = apIntCmp (/=)     (mkEval e1) (mkEval e2)
mkEval (EUnMinus _ e1    ) = fmapInt  negate   (mkEval e1)
mkEval (EUnNot   _ e1    ) = fmapBool not      (mkEval e1)
mkEval (EIf      _ e1 e2 e3) = do
    (VBool v1) <- mkEval e1
    mkEval $ if v1 then e2 else e3
mkEval (ELet      loc pt e1 e2) = do
    v <- mkEval e1
    new <- match loc (pt, v)
    vctx %= appendElmsCtx new
    v' <- mkEval e2
    vctx %= deleteElmsCtx new
    return v'

mkEval (EFieldAccess l e accs) = do
    v <- mkEval e
    access l v accs

mkEval (EActionUpdate _ e acts) = do
    v <- mkEval e
    update v acts

mkEval (ERefTrace   loc e1 e2) = do
    old <- use vctx
    (VProc v2) <- mkEval e2
    evs <- extractEvents v2
    vctx .= old
    (VProc v1) <- mkEval e1
    go evs v1
    vctx .= old
    return $ VBool True
    where
        go [] _       = return ()
        go (ev:ev1) v = do
            --dprint $ show ev
            mk <- trans ev v
            case mk of
                Just k -> go ev1 k
                Nothing -> throwError $ loc ++ "cannot trans:" ++ show ev ++ show v



mkEval (EPrefix    _ (EEvent loc "load" [PLPat _ pt]) e) = do
    v <- lookupVCtx "*global*" loc
    ctx <- match loc (pt, v)
    vctx %= appendElmsCtx ctx
    --traceShowM (loc, ctx)
    mkEval e
mkEval (EPrefix    l (EEvent _ "load" _) _) = throwError $ l ++ "invalid"
mkEval (EPrefix    _ (EEvent _ "save" [PLExp _ ee]) e) = do
    v <- mkEval ee
    vctx %= appendElmsCtx [("*global*",v)]
    mkEval e
mkEval (EPrefix    _ ev e) = return $ VProc $ VPrefix ev e
mkEval (EEChoise   _ e1 e2) = do
    ret1 <- tryEvalProcExpr e1
    ret2 <- tryEvalProcExpr e2
    return $ VProc $ VEChoise ret1 ret2
mkEval (EGuard     _ e1 e2 ) = do
    b <- mkEval e1
    case b of
        VBool True -> mkEval e2
        VBool False -> return $ VProc VStop
        _ -> error "bug"
mkEval (ESequence  _ e1 e2 ) = do
    ret1 <- tryEvalProcExpr e1
    ret2 <- tryEvalProcExpr e2
    return $ VProc $ VSequence ret1 ret2
mkEval (EInterrupt _ e1 e2 ) = do
    ret1 <- tryEvalProcExpr e1
    ret2 <- tryEvalProcExpr e2
    return $ VProc $ VInterrupt ret1 ret2
mkEval (EProcRef loc s [] ) = lookupVCtx s loc
mkEval (EProcRef loc s es ) = do
    (VProcFun defs) <- lookupVCtx s loc
    vas <- sequence $ mkEval <$> es
    -- clearContext
    e' <- foldr1 (<|>) [go def vas | def <- defs]
    mkEval e'
    where
        go (pts, e) vs = do
            new <- matchPatterns loc pts vs
            vctx %= appendElmsCtx new
            return e

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

update :: Value -> [Action] -> EvalM Value
update = foldM f
    where
    f v (Action l accs e) = do
        lns <- concatAccess l accs
        v' <- mkEval e
        return $ v & lns .~ v'

concatAccess :: Applicative f => Loc -> [Accessor] -> EvalM ((Value -> f Value) -> Value -> f Value)
concatAccess _ [] = return id
concatAccess l (Accessor _ aname : accs) = do
    (VAccessor n) <- lookupVCtx aname l
    lns <- concatAccess l accs
    return $ (_VConstr . _2 . ix n) . lns

matchEvent :: (EEvent, VEvent) -> EvalM (Maybe VCtx)
matchEvent (EEvent _ c pls, VEvent c' vs) =
    if c == c'
        then fmap concat . sequence <$> sequence [matchPayload (pl, v) | (pl,v) <- zip pls vs]
        else return Nothing

matchPayload :: (Payload, Value) -> EvalM (Maybe VCtx)
matchPayload (PLExp _ e, v) = do
    v' <- mkEval e
    if v' == v then return $ Just [] else return Nothing
matchPayload (PLPat loc p, v) = (Just <$> match loc (p, v)) `catchError` (\_ -> return Nothing)
matchPayload (PLElm loc s1 s2 s3 _ ePred, v) = do
    when (s1 /= s2 || s2 /= s3) $ throwError $ loc ++ "this syntax is not implemented"
    (do
        ctx <- use vctx
        vctx %= appendElmsCtx [(s1, v)]
        (VBool b) <- mkEval ePred
        vctx .= ctx
        return $ if b then Just [(s1, v)] else Nothing
        ) `catchError` (\_ -> return Nothing)

trans :: VEvent -> VProc -> EvalM (Maybe VProc)
trans = trans'

trans' :: VEvent -> VProc -> EvalM (Maybe VProc)
trans' ev (VPrefix  eev e) = do
    mctx <- matchEvent (eev, ev)
    case mctx of
        Nothing  -> return Nothing
        Just ctx -> do
            vctx %= appendElmsCtx ctx
            (VProc v) <- mkEval e
            return $ Just v

trans' ev (VEChoise  (ctx1, v1) (ctx2, v2)) = do
    vctx .= ctx1
    v1'  <- trans ev v1
    new1 <- use vctx
    vctx .= ctx2
    v2' <- trans ev v2
    if
       | isJust v1' && isJust v2' -> throwError "undeterministic choise"
       | isJust v1'               -> do { vctx .= new1; return v1'}
       | isJust v2'               -> return v2'
       | otherwise                -> return Nothing

trans' ev (VSequence (ctx1, v1) (ctx2, v2))      = do
    case v1 of
        VSkip -> do
            vctx .= ctx2
            trans' ev v2
        _ -> do
            vctx .= ctx1
            mv1' <- trans' ev v1
            new1 <- use vctx
            return $ mv1' >>= \v -> return $ VSequence (new1, v) (ctx2, v2)

trans' ev (VInterrupt (ctx1, v1) (ctx2, v2))      = do
    vctx .= ctx1
    v1'  <- trans ev v1
    new1 <- use vctx
    vctx .= ctx2
    v2' <- trans ev v2
    if
       | isJust v1' && isJust v2' -> throwError $ show ev ++ "undeterministic choise"
       | isJust v1'               -> do { vctx .= new1; return v1'}
       | isJust v2'               -> do { return v2'}
       | otherwise                -> return Nothing

trans' _ VStop = return Nothing
trans' _ VSkip = return Nothing

evalPayload :: Payload -> EvalM Value
evalPayload (PLExp _ e) = mkEval e
evalPayload p = throwError $ showLoc p ++ "caanot calc value"

evalEvent :: EEvent -> EvalM VEvent
evalEvent (EEvent _ c pls) = do
    vs <- sequence $ evalPayload <$> pls
    return $ VEvent c vs

extractEvents :: VProc -> EvalM [VEvent]
extractEvents VSkip = return []
extractEvents (VPrefix ee e) = do
    ev <- evalEvent ee
    VProc e' <- mkEval e
    evs <- extractEvents e'
    return $ ev:evs
extractEvents _ = throwError "invalid proc expr"

predefinedValue :: VCtx
predefinedValue = [("SKIP", VProc VSkip), ("STOP", VProc VStop)]
