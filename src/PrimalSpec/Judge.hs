{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE CPP #-}

module PrimalSpec.Judge
  ( judge
  , judgeCtx
  , judgeProc
  , predefinedType
  ) where

import PrimalSpec.Type
import PrimalSpec.Util
import Data.Set (Set, empty, insert, elems)
import Data.Maybe (fromMaybe)
import Control.Lens
import Control.Monad (when, forM, forM_, foldM)
import Control.Monad.Trans.State.Lazy (StateT, execStateT)
import Control.Monad.Except(throwError)

type TypeEq = (Loc, Type, Type)
type Solution = (Int, Type)

data Judge = Judge {
    _counter :: Int
  , _eqset   :: Set TypeEq
  , _tyctx   :: TyCtx -- TODO remove
}

makeLenses ''Judge

type JudgeM a = StateT Judge (Either String) a

runJudgeM :: TyCtx -> JudgeM a -> Either String Judge
runJudgeM ctx m = execStateT m $ Judge 0 empty ctx

newTyVar :: JudgeM Type
newTyVar = do
    c <- use counter
    counter += 1
    return $ TyVar c

newTyProc :: Int -> JudgeM Type
newTyProc 0 = return TyProc
newTyProc n = do
    vars <- sequence $ replicate n $ newTyVar
    return $ TyFun vars TyProc

judgeCtx :: TyCtx -> [(String, (Loc, [Pattern], Expr))] -> Either String TyCtx
judgeCtx ctx eactx = judge' ctx $ do
    forM_ eactx $ \(nm, (_, _, _)) -> do
        ctx' <- use tyctx
        let tys = [a | (k, a) <- ctx', k == nm]
        when (length tys == 0) $ do -- add only once
            t <- newTyVar
            tyctx %= appendCtx (nm, t)
    forM_ eactx $ \(nm, (loc, pts, e)) -> do
        t <- lookupTyCtx nm loc
        mkJudgeTop loc t pts e


judgeProc :: TyCtx -> [(String, (Loc, [Pattern], Expr))] -> Either String TyCtx
judgeProc ctx pactx = judge' ctx $ do
    forM_ pactx $ \(nm, (_, pts, _)) -> do
        ctx' <- use tyctx
        let tys = [a | (k, a) <- ctx', k == nm]
        when (null tys) $ do
            t <- newTyProc (length pts)
            tyctx %= appendCtx (nm, t)
    forM_ pactx $ \(nm, (loc, pts, e)) -> do
        t <- lookupTyCtx nm loc
        mkJudgeTop loc t pts e


judge :: TyCtx -> [(Loc, Type, [Pattern], Expr)] -> Either String TyCtx
judge ctx js = judge' ctx $ sequence_ [mkJudgeTop loc t pts e | (loc, t, pts, e) <- js]

judge' :: TyCtx -> JudgeM a -> Either String TyCtx
judge' ctx jm = do
    (Judge _ eqs ctx') <- runJudgeM ctx jm
    sols <- unify eqs
    forM_ sols $ \(v, t) -> do
        when (not $ isRigid t) $ do
            throwError $ "TyVar" ++ show v ++ " is unresolved" ++ show eqs -- TODO TyVar に位置情報持たせてここで表示
    return $ [(s, substitute sols t) | (s, t) <- ctx']

isRigid :: Type -> Bool
isRigid (TyVar _) = False
isRigid (TyFun targs tret) = all isRigid (tret:targs)
isRigid _ = True

substitute :: [(Int, Type)] -> Type -> Type
substitute sols t@(TyVar n)         = fromMaybe t $ lookup n sols
substitute sols (TyFun targs tret)  = TyFun (substitute sols <$> targs) $ substitute sols tret
substitute _    t = t


unify :: Set TypeEq -> Either String [Solution]
unify _eqs = go (elems _eqs) []
    where
    go [] sols = return sols
    go ((_, TyVar n, t):eqs) sols = if (TyVar n == t)
                                    then go eqs  sols
                                    else go eqs' sols'
        where
            repl = substitute [(n, t)]
            eqs' = [ (loc, repl l, repl r) | (loc, l,r) <- eqs]
            sols' = (n, t) : [ (i, repl t') | (i,t') <- sols]
    go ((loc, t, TyVar n):eqs) sols = go ((loc, TyVar n, t):eqs) sols
    go ((loc, TyFun l1 l2, TyFun r1 r2):eqs) sols = do
        when (length l1 /= length r1) $ throwError $ loc ++ "doesn't match num of args"
        go (zip3 (repeat loc) l1 r1 ++ ((loc, l2,r2):eqs)) sols
    go ((loc, l, r):eqs) sols = if l == r then (go eqs sols) else throwError $ loc ++ show r ++ " doen't match " ++ show l


judgeError :: Type -> Expr -> JudgeM ()
judgeError t e = throwError $ showLoc e ++ "judgement error: the expression must be " ++ show t ++ "( " ++ show e ++ " )"
--judgeError t e = error $ showLoc e ++ "judgement error: the expression must be " ++ show t ++ "( " ++ show e ++ " )"

lookupTyCtx :: String -> Loc -> JudgeM Type
lookupTyCtx k loc = do
    ctx <- use tyctx
    lookupCtx ctx k $ loc ++ k ++ " is undefined(type)"

addEq :: Loc -> Type -> Type  -> JudgeM ()
--addEq loc t1 t2 = traceShow ("EQ", t1, t2) $ do
addEq loc t1 t2 = eqset %= insert (loc, t1, t2)

mkJudgeTop :: Loc -> Type -> [Pattern] -> Expr -> JudgeM ()
mkJudgeTop _   t []  e = mkJudge' t e
mkJudgeTop loc t pts e = mkJudge' t (EAbst loc pts e)

mkJudgePattern :: Type -> Pattern -> JudgeM TyCtx
mkJudgePattern t      (PVar _ v)    = return [(v, t)]
mkJudgePattern t      (PConstr loc c pts) = do
    ct <- lookupTyCtx c loc
    ctxs <- case ct of
        TyConstr targs tret -> do
            addEq loc t tret
            forM (zip pts targs) $ \(pt, targ) -> do
                mkJudgePattern targ pt
        _ -> throwError $ loc ++ c ++ " is not a data constructor"
    return $ concat ctxs
mkJudgePattern t      (PParenthesis _ p) = mkJudgePattern t p
mkJudgePattern _      (PWildcard _) = return []
mkJudgePattern TyBool (PBool _ _) = return []
mkJudgePattern TyInt  (PInt _ _)  = return []
mkJudgePattern t@(TyVar _) (PBool loc _) = addEq loc t TyBool >> return []
mkJudgePattern t@(TyVar _) (PInt loc _)    = addEq loc t TyInt >> return []
mkJudgePattern t      p         = throwError $ show p ++ "pattern must be " ++ show t


mkJudgePayloads :: Loc -> Type -> Payload -> JudgeM TyCtx
mkJudgePayloads loc (TyProc) _  = throwError $ loc ++ "Proc cannot be used as payload"
mkJudgePayloads loc (TyEvent) _ = throwError $ loc ++ "Event cannot be used as payload"
mkJudgePayloads _   t (PLExp _   e            ) = mkJudge t e >> return []
mkJudgePayloads _   t (PLPat _   p            ) = mkJudgePattern t p
mkJudgePayloads _   t (PLElm loc s1 s2 s3 t' e) = do
    when (s1 /= s2 || s2 /= s3) $ throwError $ loc ++ "constraint vars should be same name"
    let new = [(s1, t')]
    old <- use tyctx
    tyctx %= appendElmsCtx new
    mkJudge TyBool e
    tyctx .= old
    addEq loc t t'
    return new


squashType :: [Accessor] -> JudgeM (Type, Type)
squashType as = do
    ts <- forM as $ \(Accessor l aname) -> do
        TyAccessor r t <- lookupTyCtx aname l
        return (l,r,t)
    go ts
    where
    go [] = error "empty accessor"
    go ((_,r,t):ts) = do
        t' <- go' t ts
        return (r, t')
    go' t0 [] = return t0
    go' t0 ((l,r,t):ts) = do
        when (t0 /= r) $ throwError $ l ++ "type mismatch" ++ show (t0, r)
        go' t ts


mkJudge' :: Type -> Expr -> JudgeM ()
mkJudge' t e = do
    dlogM JUDGE $ show (t, e)
    mkJudge t e

mkJudge :: Type -> Expr -> JudgeM ()
mkJudge t      (EVar          loc v   ) = lookupTyCtx v loc >>= addEq loc t
mkJudge t      (EId           loc v   ) = do
    ct <- lookupTyCtx v loc
    case ct of
        TyProc -> addEq loc t TyProc
        TyConstr [] tret -> do
            addEq loc t tret
        _ -> throwError $ loc ++ v ++ " should be a data constructor or a proc"
mkJudge t      (EConstr       loc c as) = do
    dlogM JUDGE c
    ct <- lookupTyCtx c loc
    dlogM JUDGE $ show ct
    case ct of
        TyConstr targs tret -> do
            addEq loc t tret
            when (length as /= length targs) $ throwError $ loc ++ c ++ " doesn't match num of args"
            forM_ (zip as targs) $ \(a, targ) -> do
                mkJudge' targ a
        _ -> throwError $ loc ++ c ++ " is not a data constructor"
mkJudge t      (EProcRef       loc c as) = do
    ct <- lookupTyCtx c loc
    case ct of
        TyProc -> addEq loc t TyProc
        TyFun targs tret -> do
            addEq loc t tret
            when (length as /= length targs) $ throwError $ loc ++ c ++ " doesn't match num of args"
            forM_ (zip as targs) $ \(a, targ) -> do
                mkJudge' targ a
        _ -> throwError $ loc ++ c ++ " is not a proc"
mkJudge t      (EParenthesis _ e ) = mkJudge' t e
mkJudge t      (EApply        _   f as) = do
    vs <- forM as $ \a -> do
        v <- newTyVar
        mkJudge' v a
        return v
    mkJudge' (TyFun vs t) f
mkJudge t      (ELet          _   pt e1 e2) = do
    old <- use tyctx
    v <- newTyVar
    new <- mkJudgePattern v pt
    mkJudge' v e1
    tyctx %= appendElmsCtx new
    mkJudge' t e2
    tyctx .= old
mkJudge t      (EIf           _   c e1 e2)  = mkJudge' TyBool c >> mkJudge' t     e1 >> mkJudge' t     e2

mkJudge t      (EFieldAccess  loc   e accs)   = do
    (recType, targetType) <- squashType accs
    mkJudge' recType e
    addEq loc t targetType

mkJudge t (EActionUpdate _ e acts)   = do
    mkJudge' t e
    forM_ acts $ \(Action l accs e') -> do
        (recType, targetType) <- squashType accs
        addEq l t recType
        mkJudge' targetType e'
mkJudge (TyData tnm) e = throwError $ showLoc e ++ "type should be " ++ tnm
mkJudge TyEvent  e = throwError  $ showLoc e ++ "type should be Event"

mkJudge (TyAccessor _ _) _ = error "invalid operation"

mkJudge TyInt  (EInt          _   _   ) = return ()
mkJudge TyInt  (ETimes        _   l r ) = mkJudge' TyInt l  >> mkJudge' TyInt r
mkJudge TyInt  (EDiv          _   l r ) = mkJudge' TyInt l  >> mkJudge' TyInt r
mkJudge TyInt  (EMod          _   l r ) = mkJudge' TyInt l  >> mkJudge' TyInt r
mkJudge TyInt  (EPower        _   l r ) = mkJudge' TyInt l  >> mkJudge' TyInt r
mkJudge TyInt  (EPlus         _   l r ) = mkJudge' TyInt l  >> mkJudge' TyInt r
mkJudge TyInt  (EMinus        _   l r ) = mkJudge' TyInt l  >> mkJudge' TyInt r
mkJudge TyInt  (EUnMinus      _   e   ) = mkJudge TyInt e
mkJudge TyInt  e                        = judgeError TyInt e

mkJudge TyBool (EBool         _ _     ) = return ()
mkJudge TyBool (EAnd          _ l r   ) = mkJudge' TyBool l  >> mkJudge' TyBool r
mkJudge TyBool (EOr           _ l r   ) = mkJudge' TyBool l  >> mkJudge' TyBool r
mkJudge TyBool (ELT           _ l r   ) = mkJudge' TyInt  l  >> mkJudge' TyInt  r
mkJudge TyBool (EGT           _ l r   ) = mkJudge' TyInt  l  >> mkJudge' TyInt  r
mkJudge TyBool (ELE           _ l r   ) = mkJudge' TyInt  l  >> mkJudge' TyInt  r
mkJudge TyBool (EGE           _ l r   ) = mkJudge' TyInt  l  >> mkJudge' TyInt  r
mkJudge TyBool (EEQ           _ l r   ) = do
    v <- newTyVar
    mkJudge v l
    mkJudge v r
mkJudge TyBool (ENE           _ l r   ) = do
    v <- newTyVar
    mkJudge v l
    mkJudge v r
mkJudge TyBool (EUnNot        _ e     ) = mkJudge' TyBool e
mkJudge TyBool (ERefTrace _ pe1 pe2)    = do
    mkJudge' TyProc pe1
    mkJudge' TyProc pe2
mkJudge TyBool e                        = judgeError TyBool e

mkJudge (TyFun targs tret) (EAbst  _ pts e) = do
    when (length pts /= length targs) $ throwError $ showLoc e ++ "doesn't match num of args"
    old <- use tyctx
    new <- concat <$> (sequence $ go <$> zip pts targs)
    tyctx %= appendElmsCtx new
    mkJudge' tret e
    tyctx .= old
    where
        go (pt, targ) = mkJudgePattern targ pt

mkJudge t@(TyFun _ _) e                 = judgeError t e

mkJudge t@(TyVar _) (EInt          loc   _   ) = addEq loc t TyInt
mkJudge t@(TyVar _) (ETimes        loc   l r ) = addEq loc t TyInt >> mkJudge' TyInt l  >> mkJudge' TyInt r
mkJudge t@(TyVar _) (EDiv          loc   l r ) = addEq loc t TyInt >> mkJudge' TyInt l  >> mkJudge' TyInt r
mkJudge t@(TyVar _) (EMod          loc   l r ) = addEq loc t TyInt >> mkJudge' TyInt l  >> mkJudge' TyInt r
mkJudge t@(TyVar _) (EPower        loc   l r ) = addEq loc t TyInt >> mkJudge' TyInt l  >> mkJudge' TyInt r
mkJudge t@(TyVar _) (EPlus         loc   l r ) = addEq loc t TyInt >> mkJudge' TyInt l  >> mkJudge' TyInt r
mkJudge t@(TyVar _) (EMinus        loc   l r ) = addEq loc t TyInt >> mkJudge' TyInt l  >> mkJudge' TyInt r
mkJudge t@(TyVar _) (EUnMinus      loc   e   ) = addEq loc t TyInt >> mkJudge' TyInt e
mkJudge t@(TyVar _) (EBool         loc _     ) = addEq loc t TyBool >> return ()
mkJudge t@(TyVar _) (EAnd          loc l r   ) = addEq loc t TyBool >> mkJudge' TyBool l  >> mkJudge' TyBool r
mkJudge t@(TyVar _) (EOr           loc l r   ) = addEq loc t TyBool >> mkJudge' TyBool l  >> mkJudge' TyBool r
mkJudge t@(TyVar _) (ELT           loc l r   ) = addEq loc t TyBool >> mkJudge' TyBool l  >> mkJudge' TyBool r
mkJudge t@(TyVar _) (EGT           loc l r   ) = addEq loc t TyBool >> mkJudge' TyBool l  >> mkJudge' TyBool r
mkJudge t@(TyVar _) (ELE           loc l r   ) = addEq loc t TyBool >> mkJudge' TyBool l  >> mkJudge' TyBool r
mkJudge t@(TyVar _) (EGE           loc l r   ) = addEq loc t TyBool >> mkJudge' TyBool l  >> mkJudge' TyBool r
mkJudge t@(TyVar _) (EEQ           loc l r   ) = addEq loc t TyBool >> mkJudge' TyBool l  >> mkJudge' TyBool r
mkJudge t@(TyVar _) (ENE           loc l r   ) = addEq loc t TyBool >> mkJudge' TyBool l  >> mkJudge' TyBool r
mkJudge t@(TyVar _) (EUnNot        loc e     ) = addEq loc t TyBool >> mkJudge' TyBool e
mkJudge t@(TyVar _) (EAbst         loc pts e ) = do
    old <- use tyctx
    (ts, new) <- foldM go ([], []) pts
    v' <- newTyVar
    addEq loc (TyFun ts v') t
    tyctx %= appendElmsCtx new
    mkJudge' v' e
    tyctx .= old
    where
        go (vs, elms) pt = do
            v <- newTyVar
            c' <- mkJudgePattern v pt
            return (vs ++ [v], c' ++ elms)
mkJudge (TyVar nm) e = throwError $  showLoc e ++ "TyVar" ++ show nm ++ "is unresolved"
mkJudge (TyConstr _ _) e = throwError $ showLoc e ++ "type should be constructor"

mkJudge TyProc (EPrefix    _ (EEvent loc c as) pe me) = do
    TyConstr targs _ <- lookupTyCtx c loc
    when (length as /= length targs) $ throwError $ loc ++ c ++ " doesn't match num of args"
    old <- use tyctx
    news <- forM (zip as targs) $ \(a, targ) -> do
        mkJudgePayloads loc targ a
    tyctx %= (appendElmsCtx $ concat news)
    case me of
        Just e -> mkJudge' (TyData "Global") e
        _ -> return ()
    mkJudge' TyProc pe
    tyctx .= old

mkJudge TyProc (EGuard   _ be pe)    = do
    mkJudge' TyBool be
    mkJudge' TyProc pe
mkJudge TyProc (EEChoise   _ pe1 pe2)    = do
    mkJudge' TyProc pe1
    mkJudge' TyProc pe2
mkJudge TyProc (ESequence  _ pe1 pe2)    = do
    mkJudge' TyProc pe1
    mkJudge' TyProc pe2
mkJudge TyProc (EInterrupt _ pe1 pe2)    = do
    mkJudge' TyProc pe1
    mkJudge' TyProc pe2
mkJudge TyProc e = throwError $ showLoc e ++ "type should be Proc"


predefinedType :: TyCtx
predefinedType = [ ("SKIP", TyProc) , ("STOP", TyProc), ("global", TyData "Global") ]


