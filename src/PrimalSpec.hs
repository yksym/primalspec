{-# LANGUAGE OverloadedStrings #-}
module PrimalSpec
  ( runLexer
  , runParser
  , checkAllAssert
  , module PrimalSpec.Type
  , module PrimalSpec.Util
  ) where

import PrimalSpec.Lexer
import PrimalSpec.Parser
import PrimalSpec.Eval
import PrimalSpec.Judge
import PrimalSpec.Type
import PrimalSpec.Util
import Control.Lens
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad(forM, forM_, when, unless)
import Control.Monad.Except(throwError)

checkAllAssert :: Program -> String
checkAllAssert (Program _ vctx es) = case sequence_ [go vctx e | e <- es] of
    Right () -> "assertion success"
    Left s -> s
    where
    go c e = do
        (VBool b) <- evalExpr c e
        unless b $ throwError $ showLoc e ++ "assertion failure"


assertNoMultiDefinition :: DefCtx -> Either String ()
assertNoMultiDefinition ctx = forM_ ctx $ \(nm, _) -> do
        let vs = [a | (k, a) <- ctx, k == nm ]

        when (length vs >= 2) $ do
            let (loc, _, _) = head $ last vs
            throwError $ loc ++ nm ++ ": multi-defined"


-- no constructor in vctx
valueContext :: [Stmt] -> Either String VCtx
valueContext stmts = do
    let eactx  = [(nm, (loc, ps, e)) | (loc, nm, ps, e)  <- stmts ^.. traverse . _SExprAssign]
    let ectx = groupCtx eactx
    assertNoMultiDefinition ectx
    forM ectx $ \(nm, ds) -> do
        let v = case ds of
                    [(_, [], e) ] -> VThunk e
                    [(_, pts, e)] -> VFun [(pts, e)]
                    _             -> VFun [(pts, e) | (_, pts, e) <- ds]
        return (nm, v)

procContext :: [Stmt] -> Either String VCtx
procContext stmts = do
    let pactx  = [(nm, (loc, ps, e)) | (loc, nm, ps, e)  <- stmts ^.. traverse . _SProcAssign]
    let pctx = groupCtx pactx
    assertNoMultiDefinition pctx
    forM pctx $ \(nm, ds) -> do
        let v = case ds of
                    [(_, [], e) ] -> VThunk e
                    [(_, pts, e)] -> VProcFun [(pts, e)]
                    _             -> VProcFun [(pts, e) | (_, pts, e) <- ds]
        return (nm, v)

typeContext :: [Stmt] -> Either String TyCtx
typeContext stmts = do

    let tyctx0  = [(nm, te) | (_, nm, te)  <- stmts ^.. traverse . _STypeDecl]
                 ++ concat [constructType nm cs | (_, nm, cs)  <- stmts ^.. traverse . _SDataTypeDecl]
                 ++ concat [recordConstructType nm fds | (_, nm, fds)  <- stmts ^.. traverse . _SRecordTypeDecl]
                 ++ concat [channelType nm ts |  (_, nm, ts) <- stmts ^.. traverse . _SEventDecl ]
                 ++ predefinedType

    let eactx  = [(nm, (loc, ps, e)) | (loc, nm, ps, e)  <- stmts ^.. traverse . _SExprAssign]
    tyctx1 <- judgeCtx tyctx0 eactx

    let pactx  = [(nm, (loc, ps, e)) | (loc, nm, ps, e)  <- stmts ^.. traverse . _SProcAssign]
    tyctx <- judgeProc tyctx1 pactx

    forM_ tyctx $ \(nm, _) -> do
        let nkey = length $ tyctx ^.. traverse . _1 . filtered (nm==)
        when (nkey >= 2) $ throwError $ nm ++ ": multi-defined"

    return tyctx


runParser :: String -> Either String Program
runParser s = do
    stmts <- runAlex (B.pack s) prspParser

    tyctx <- typeContext stmts
    dlogM DEBUG $ show tyctx
    vctx <- valueContext stmts
    dlogM DEBUG $ show vctx
    pctx <- procContext stmts
    let fvctx   = [(nm, VAccessor i) | (_, _, fds)  <- stmts ^.. traverse . _SRecordTypeDecl, (i, FieldDecl _ nm _) <- zip [0..] fds]
    let asexprs = stmts ^.. traverse . _SAssert . _2

    forM_ asexprs $ \e -> judge tyctx [(showLoc e, TyBool, [], e)]

    return $ Program tyctx (vctx ++ fvctx ++ pctx ++ predefinedValue) asexprs

runLexer :: String -> Either String  [TokenClass]
runLexer s = runAlex (B.pack s) scan
    where
    scan = go []
    go xs = do
        (Token _ x) <- alexMonadScan
        if x == TokenEOF then return xs
                         else go $ xs ++ [x]

