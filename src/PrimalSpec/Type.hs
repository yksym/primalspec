{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module PrimalSpec.Type
  ( module PrimalSpec.Type
  ) where

import Data.List(groupBy, lookup)
--import Data.Monoid((++))
import Control.Lens
import Control.Monad.Except(throwError, MonadError)
import GHC.Stack (HasCallStack)

type Loc = String -- TODO: Loc filepath line col

class ShowLoc a where
    showLoc :: a -> Loc

data Pattern
    = PInt Loc Int
    | PBool Loc Bool
    | PWildcard Loc
    | PVar Loc String
    | PParenthesis Loc Pattern
    | PConstr Loc String [Pattern]
    deriving (Eq, Show)

makePrisms ''Pattern

instance ShowLoc Pattern where
    showLoc (PInt loc _) = loc
    showLoc (PBool loc _) = loc
    showLoc (PWildcard loc) = loc
    showLoc (PVar loc _) = loc
    showLoc (PParenthesis loc _) = loc
    showLoc (PConstr loc _ _) = loc

data Accessor = Accessor Loc String deriving (Eq, Show)
data Action = Action Loc [Accessor] Expr deriving (Show)

data EEvent = EEvent Loc String [Payload]


instance ShowLoc EEvent where
    showLoc (EEvent l _ _) = l

instance Show EEvent where
    show (EEvent l c _) = c ++ "(" ++ l ++ ")"

-- channel constructor :: T -> TyEvent
data Type
    = TyBool
    | TyInt
    | TyFun [Type] Type
    | TyConstr [Type] Type
    | TyVar Int
    | TyData String
    | TyAccessor Type Type
    | TyProc
    | TyEvent
    -- | TyList Type
    -- | TySet Type
    deriving (Eq, Ord, Show)

data Payload
    = PLExp Loc Expr
    | PLPat Loc Pattern
    | PLElm Loc String String String Type Expr
    deriving (Show)

instance ShowLoc Payload where
    showLoc (PLExp loc _ ) = loc
    showLoc (PLPat loc _ ) = loc
    showLoc (PLElm loc _ _ _ _ _) = loc

data Expr
    = EInt Loc Int
    | EBool Loc Bool
    | EVar Loc String
    | EId  Loc String
    | EParenthesis Loc Expr
    | EAbst Loc [Pattern] Expr
    | EApply Loc Expr [Expr]
    | EConstr Loc String [Expr]
    | EProcRef Loc String [Expr]
    | ETimes Loc Expr Expr
    | EDiv Loc Expr Expr
    | EMod Loc Expr Expr
    | EPower Loc Expr Expr
    | EPlus Loc Expr Expr
    | EMinus Loc Expr Expr
    | EAnd Loc Expr Expr
    | EOr  Loc Expr Expr
    | ELT  Loc Expr Expr
    | EGT  Loc Expr Expr
    | ELE  Loc Expr Expr
    | EGE  Loc Expr Expr
    | EEQ  Loc Expr Expr
    | ENE  Loc Expr Expr
    | EUnMinus Loc Expr
    | EUnNot Loc Expr
    | ELet Loc Pattern Expr Expr
    | EIf  Loc Expr Expr Expr
    | ERefTrace Loc Expr Expr
    | EActionUpdate Loc Expr [Action]
    | EFieldAccess Loc Expr [Accessor]
    | EPrefix    Loc EEvent Expr (Maybe Expr)
    | EGuard     Loc Expr Expr
    | EEChoise   Loc Expr Expr
    | ESequence  Loc Expr Expr
    | EInterrupt Loc Expr Expr
    deriving (Show)

makePrisms ''Expr

instance ShowLoc Expr where
    showLoc ( EInt loc _          ) = loc
    showLoc ( EBool loc _         ) = loc
    showLoc ( EVar loc _          ) = loc
    showLoc ( EId  loc _          ) = loc
    showLoc ( EParenthesis loc _  ) = loc
    showLoc ( EAbst loc _ _       ) = loc
    showLoc ( EApply loc _ _      ) = loc
    showLoc ( EConstr loc _ _     ) = loc
    showLoc ( EProcRef loc _ _       ) = loc
    showLoc ( ETimes loc _ _      ) = loc
    showLoc ( EDiv loc _ _        ) = loc
    showLoc ( EMod loc _ _        ) = loc
    showLoc ( EPower loc _ _      ) = loc
    showLoc ( EPlus loc _ _       ) = loc
    showLoc ( EMinus loc _ _      ) = loc
    showLoc ( EAnd loc _ _        ) = loc
    showLoc ( EOr  loc _ _        ) = loc
    showLoc ( ELT  loc _ _        ) = loc
    showLoc ( EGT  loc _ _        ) = loc
    showLoc ( ELE  loc _ _        ) = loc
    showLoc ( EGE  loc _ _        ) = loc
    showLoc ( EEQ  loc _ _        ) = loc
    showLoc ( ENE  loc _ _        ) = loc
    showLoc ( EUnMinus loc _      ) = loc
    showLoc ( EUnNot loc _        ) = loc
    showLoc ( ELet loc _ _ _      ) = loc
    showLoc ( EIf  loc _ _ _      ) = loc
    showLoc ( ERefTrace  loc _ _  ) = loc
    showLoc ( EActionUpdate loc _ _) = loc
    showLoc ( EFieldAccess loc _ _) = loc
    showLoc ( EPrefix    loc _ _   _) = loc
    showLoc ( EEChoise   loc _ _  ) = loc
    showLoc ( EGuard     loc _ _  ) = loc
    showLoc ( ESequence  loc _ _  ) = loc
    showLoc ( EInterrupt loc _ _  ) = loc

type Ctx a = [(String, a)]

emptyCtx :: Ctx a
emptyCtx = []

appendCtx :: (String, a) -> Ctx a -> Ctx a
appendCtx (n, a) c = (n,a) : c

appendElmsCtx :: Ctx a -> Ctx a -> Ctx a
appendElmsCtx target ctx = foldr appendCtx ctx target


lookupCtx :: (MonadError String m, Show a) => Ctx a -> String -> String -> m a
lookupCtx ctx key msg = do
    case lookup key ctx of
        Just x -> return x
        Nothing -> throwError msg

groupCtx :: Ctx a -> Ctx [a]
groupCtx ctx = [squash sameKeyValues | sameKeyValues <- groupBy (\x y -> x^._1 == y^._1) ctx]
    where squash ctxs = ((head ctxs)^._1, ctxs ^.. traverse . _2)

updateCtx :: (String, a) -> Ctx a -> Ctx a
updateCtx _ [] = []
updateCtx c@(k, _) (c'@(k',_):cs) = if k == k' then c:cs else c':(updateCtx c cs)

type VCtx  = Ctx Value

data VProc
    = VPrefix    EEvent Expr (Maybe Expr)
    | VSequence  (VCtx, VProc) (VCtx, Expr)
    | VInterrupt (VCtx, VProc) (VCtx, VProc)
    | VEChoise   (VCtx, VProc) (VCtx, VProc)
    | VSkip
    | VStop
    -- | VChaos

instance Show VProc where
    show = go [] where
        go bs (VPrefix ev _ _)               = indent bs ++ show ev ++ " -> ..." ++ endl
        go bs (VSequence  (_, p1) (_, _)) = indent bs ++ ";"  ++ endl ++ go (bs++[True]) p1 ++ (indent (bs++[False]) ++ "...")
        go bs (VInterrupt (_, p1) (_, p2)) = indent bs ++ "/\\" ++ endl  ++ go (bs++[True]) p1 ++ go (bs++[False]) p2
        go bs (VEChoise   (_,p1) (_,p2))   = indent bs ++ "[]"  ++ endl ++ go (bs++[True]) p1 ++ go (bs++[False]) p2
        go bs VSkip                      = indent bs ++ "SKIP" ++ endl
        go bs VStop                      = indent bs ++ "STOP" ++ endl
        indent [] = "--* "
        indent [True] = "  |----* "
        indent [False] = "  `----* "
        indent (b:bs) = (if b then "  |  " else "     ") ++ indent bs
        endl = "\n"

data Value
    = VBool Bool
    | VInt  Int
    | VClosure [Pattern] [(String, Value)] Expr
    | VFun [([Pattern], Expr)]
    | VThunk Expr
    | VConstr String [Value] -- TODO VRecord
    | VAccessor Int
    | VProc VProc
    | VProcFun [([Pattern], Expr)]
    deriving (Show)

makePrisms ''Value

eqValue :: (HasCallStack) => Value -> Value -> Bool
eqValue (VBool v1) (VBool v2) = v1 == v2
eqValue (VInt  v1) (VInt  v2) = v1 == v2
eqValue (VConstr c1 vs1) (VConstr c2 vs2) = c1 == c2 && (all id $ zipWith eqValue vs1 vs2)
eqValue v1 v2 = error $ "invalid argument!!\n" ++ show v1 ++ "\n" ++ show v2

nqValue :: (HasCallStack) => Value -> Value -> Bool
nqValue x y = not $ eqValue x y

data VEvent = VEvent String [Value] deriving (Show)

type TyCtx = Ctx Type
type EACtx = Ctx (Loc, [Pattern], Expr)
type DefCtx = Ctx [(Loc, [Pattern], Expr)]

data DataConstrDecl = DataConstrDecl String [Type] deriving (Eq, Show)

data FieldDecl = FieldDecl Loc String Type deriving (Eq, Show)

data Stmt
    = SProcAssign Loc String [Pattern] Expr
    | SExprAssign Loc String [Pattern] Expr
    | STypeDecl   Loc String Type
    | SAssert Loc Expr
    | SEventDecl Loc String [Type]
    | SDataTypeDecl Loc String [DataConstrDecl]
    | SRecordTypeDecl Loc String [FieldDecl]
    deriving (Show)

makePrisms ''Stmt


data Program = Program {
    _tyCtx :: TyCtx
  , _vCtx  :: VCtx
  , _assertList :: [Expr]
} deriving (Show)

makeLenses ''Program

constructType :: String -> [DataConstrDecl] -> TyCtx
constructType tname ds = [(cname, TyConstr targs $ TyData tname) | (DataConstrDecl cname targs) <- ds]


recordConstructType :: String -> [FieldDecl] -> TyCtx
recordConstructType tname fds = (tname, ctype) : [(fname, TyAccessor (TyData tname) t) | FieldDecl _ fname t <- fds]
    where
        ctype = TyConstr [t | FieldDecl _ _ t <- fds] $ TyData tname


channelType :: String -> [Type] -> TyCtx
channelType name ts = [(name, TyConstr ts TyEvent)]

