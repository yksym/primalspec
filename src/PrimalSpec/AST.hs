{-# LANGUAGE LambdaCase, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- Haskell AST
module PrimalSpec.AST where

import DynFlags (xopt)
import GHC hiding (loadModule, ParsedModule)
import GHC.Paths ( libdir )
--import Module as GHC (mkModuleName)
import StringBuffer (hGetStringBuffer)

--import Control.Monad (Monad(..), mapM, (=<<))
import Control.Monad.IO.Class (MonadIO(..))
--import Data.List
import Data.Maybe (Maybe(..), fromJust)
import Language.Haskell.TH.LanguageExtensions (Extension(..))
--import System.Directory (listDirectory)
--import System.FilePath
--import System.IO

import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.BackendGHC (runTrf, trfModule)
import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.PrettyPrint.Prepare
import Language.Haskell.Tools.Refactor
--import Language.Haskell.Tools.AST as AST (SrcTemplateStage, Ann, SourceInfoTraversal)
--import Language.Haskell.Tools.Rewrite as G

-- | Show instance for Haskell AST representation ignoring source and semantic information

import Language.Haskell.Tools.AST.Ann (Ann(..), AnnListG(..), AnnMaybeG(..))

-- Annotations
instance (Show (e dom stage)) => Show (Ann e dom stage) where
  show (Ann _ e) = show e

instance (Show (e dom stage)) => Show (AnnMaybeG e dom stage) where
  show (AnnMaybeG _ e) = show e

instance (Show (e dom stage)) => Show (AnnListG e dom stage) where
  show (AnnListG _ e) = show e

-- Modules
deriving instance Show (UModule dom stage)
deriving instance Show (UModuleHead dom stage)
deriving instance Show (UExportSpecs dom stage)
deriving instance Show (UExportSpec dom stage)
deriving instance Show (UIESpec dom stage)
deriving instance Show (USubSpec dom stage)
deriving instance Show (UModulePragma dom stage)
deriving instance Show (UFilePragma dom stage)
deriving instance Show (UImportDecl dom stage)
deriving instance Show (UImportSpec dom stage)
deriving instance Show (UImportModifier dom stage)
deriving instance Show (UImportQualified dom stage)
deriving instance Show (UImportSource dom stage)
deriving instance Show (UImportSafe dom stage)
deriving instance Show (UTypeNamespace dom stage)
deriving instance Show (UImportRenaming dom stage)

-- Declarations
deriving instance Show (UDecl dom stage)
deriving instance Show (UClassBody dom stage)
deriving instance Show (UClassElement dom stage)
deriving instance Show (UDeclHead dom stage)
deriving instance Show (UInstBody dom stage)
deriving instance Show (UInstBodyDecl dom stage)
deriving instance Show (UGadtConDecl dom stage)
deriving instance Show (UGadtConType dom stage)
deriving instance Show (UFieldWildcard dom stage)
deriving instance Show (UFunDeps dom stage)
deriving instance Show (UFunDep dom stage)
deriving instance Show (UConDecl dom stage)
deriving instance Show (UFieldDecl dom stage)
deriving instance Show (UDeriving dom stage)
deriving instance Show (UDeriveStrategy dom stage)
deriving instance Show (UInstanceRule dom stage)
deriving instance Show (UInstanceHead dom stage)
deriving instance Show (UTypeEqn dom stage)
deriving instance Show (UKindConstraint dom stage)
deriving instance Show (UTyVar dom stage)
deriving instance Show (UType dom stage)
deriving instance Show (UKind dom stage)
deriving instance Show (UContext dom stage)
deriving instance Show (UAssertion dom stage)
deriving instance Show (UExpr dom stage)
deriving instance Show (expr dom stage) => Show (UStmt' expr dom stage)
deriving instance Show (UCompStmt dom stage)
deriving instance Show (UValueBind dom stage)
deriving instance Show (UPattern dom stage)
deriving instance Show (UPatternField dom stage)
deriving instance Show (USplice dom stage)
deriving instance Show (QQString dom stage)
deriving instance Show (UMatch dom stage)
deriving instance Show (expr dom stage) => Show (UAlt' expr dom stage)
deriving instance Show (URhs dom stage)
deriving instance Show (UGuardedRhs dom stage)
deriving instance Show (UFieldUpdate dom stage)
deriving instance Show (UBracket dom stage)
deriving instance Show (UTopLevelPragma dom stage)
deriving instance Show (URule dom stage)
deriving instance Show (URuleVar dom stage)
deriving instance Show (UAnnotationSubject dom stage)
deriving instance Show (UMinimalFormula dom stage)
deriving instance Show (UExprPragma dom stage)
deriving instance Show (USourceRange dom stage)
deriving instance Show (Number dom stage)
deriving instance Show (UQuasiQuote dom stage)
deriving instance Show (URhsGuard dom stage)
deriving instance Show (ULocalBind dom stage)
deriving instance Show (ULocalBinds dom stage)
deriving instance Show (UFixitySignature dom stage)
deriving instance Show (UTypeSignature dom stage)
deriving instance Show (UListCompBody dom stage)
deriving instance Show (UTupSecElem dom stage)
deriving instance Show (UTypeFamily dom stage)
deriving instance Show (UTypeFamilySpec dom stage)
deriving instance Show (UInjectivityAnn dom stage)
deriving instance Show (expr dom stage) => Show (UCaseRhs' expr dom stage)
deriving instance Show (expr dom stage) => Show (UGuardedCaseRhs' expr dom stage)
deriving instance Show (UPatternSynonym dom stage)
deriving instance Show (UPatSynRhs dom stage)
deriving instance Show (UPatSynLhs dom stage)
deriving instance Show (UPatSynWhere dom stage)
deriving instance Show (UPatternTypeSignature dom stage)
deriving instance Show (URole dom stage)
deriving instance Show (UCmd dom stage)
deriving instance Show (ULanguageExtension dom stage)
deriving instance Show (UMatchLhs dom stage)
deriving instance Show (UInlinePragma dom stage)
deriving instance Show (USpecializePragma dom stage)
deriving instance Show (UUnboxedSumPlaceHolder dom stage)


-- ULiteral
deriving instance Show (ULiteral dom stage)
deriving instance Show (k dom stage) => Show (UPromoted k dom stage)

-- Base
deriving instance Show (UOperator dom stage)
deriving instance Show (UName dom stage)
deriving instance Show (UQualifiedName dom stage)
deriving instance Show (UModuleName dom stage)
deriving instance Show (UNamePart dom stage)
deriving instance Show (UStringNode dom stage)
deriving instance Show (UDataOrNewtypeKeyword dom stage)
deriving instance Show (UDoKind dom stage)
deriving instance Show (TypeKeyword dom stage)
deriving instance Show (UOverlapPragma dom stage)
deriving instance Show (UCallConv dom stage)
deriving instance Show (UArrowAppl dom stage)
deriving instance Show (USafety dom stage)
deriving instance Show (UConlikeAnnot dom stage)
deriving instance Show (Assoc dom stage)
deriving instance Show (Precedence dom stage)
deriving instance Show (LineNumber dom stage)
deriving instance Show (UPhaseControl dom stage)
deriving instance Show (PhaseNumber dom stage)
deriving instance Show (PhaseInvert dom stage)


--main :: IO ()
--main = do
--        --checkCorrectlyPrinted "." "Huga"
--        let hoge = mkNormalName $ mkQualifiedName ["A"] "b"
--        print hoge
--        return ()

checkCorrectlyPrinted :: String -> String -> IO ()
checkCorrectlyPrinted workingDir mname
  = do
       --expectedHandle <- openBinaryFile (workingDir </> map (\case '.' -> pathSeparator; c -> c) mname ++ ".hs") ReadMode
       --expected <- hGetContents expectedHandle
       (ast, actual) <- runGhc (Just libdir) $ do
                     parsed <- loadModule workingDir mname
                     ast <- parseAST parsed
                     let actual = prettyPrint ast
                     return (ast, actual)
       print ast
       print actual
       return ()


parseAST :: ModSummary -> Ghc (Ann UModule (Dom RdrName) SrcTemplateStage)
parseAST modSum = do
  let hasStaticFlags = StaticPointers `xopt` ms_hspp_opts modSum
      hasCppExtension = Cpp `xopt` ms_hspp_opts modSum
      ms = if hasStaticFlags then forceAsmGen modSum else modSum
  p <- parseModule ms
  sourceOrigin <- if hasCppExtension then liftIO $ hGetStringBuffer (getModSumOrig ms)
                                     else return (fromJust $ ms_hspp_buf $ pm_mod_summary p)
  let annots = pm_annotations p
  (if hasCppExtension then prepareASTCpp else prepareAST) sourceOrigin . placeComments (fst annots) (getNormalComments $ snd annots)
     <$> (runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule ms $ pm_parsed_source p)



--
--main :: IO ()
--main = defaultMain genTests
--
--genTests :: TestTree
--genTests = testGroup "ast generation tests"
--             [ testGroup "name tests" testBase
--             , testGroup "expression tests" (map makeGenTest testExprs)
--             , testGroup "pattern tests" (map makeGenTest testPatterns)
--             , testGroup "type tests" (map makeGenTest testType)
--             , testGroup "binding tests" (map makeGenTest testBinds)
--             , testGroup "declaration tests" (map makeGenTest testDecls)
--             , testGroup "module tests" (map makeGenTest testModules)
--             ]
--
--makeGenTest :: SourceInfoTraversal elem => (String, String, Ann elem dom SrcTemplateStage) -> TestTree
--makeGenTest (name, expected, ast) = testCase name $ assertEqual "The generated AST is not what is expected" expected (prettyPrint ast)
--
--testBase
--  = [ makeGenTest ("qualified name", "A.b", mkNormalName $ mkQualifiedName ["A"] "b")
--    , makeGenTest ("qualified operator", "A.+", mkQualOp ["A"] "+")
--    , makeGenTest ("backtick operator", "`mod`", mkBacktickOp [] "mod")
--    , makeGenTest ("operator name", "(+)", mkParenName $ mkSimpleName "+")
--    ]
--
--testExprs
--  = [ ("infix", "a + 3", mkInfixApp (mkVar (mkName "a")) (mkUnqualOp "+") (mkLit $ mkIntLit 3))
--    , ("section", "(\"xx\" ++)", mkLeftSection (mkLit (mkStringLit "xx")) (mkUnqualOp "++"))
--    , ("tuple", "(1, [2, 3])", mkTuple [ mkLit (mkIntLit 1), mkList [ mkLit (mkIntLit 2), mkLit (mkIntLit 3) ] ])
--    , ("record constructor", "P { x = 1 }", mkRecCon (mkName "P") [ mkFieldUpdate (mkName "x") (mkLit $ mkIntLit 1) ])
--    , ("if", "if f a then x else y"
--      , mkIf (mkApp (mkVar $ mkName "f") (mkVar $ mkName "a")) (mkVar $ mkName "x") (mkVar $ mkName "y"))
--    , ("let", "let nat = [0..] in !z"
--      , mkLet [mkLocalValBind $ mkSimpleBind' (mkName "nat") (mkEnum (mkLit (mkIntLit 0)) Nothing Nothing)]
--                                              (mkPrefixApp (mkUnqualOp "!") (mkVar $ mkName "z")) )
--    , ("case",   "case x of Just y -> y\n"
--              ++ "          Nothing -> 0"
--      , mkCase (mkVar (mkName "x"))
--          [ mkAlt (mkAppPat (mkName "Just") [mkVarPat (mkName "y")]) (mkCaseRhs $ mkVar (mkName "y")) Nothing
--          , mkAlt (mkVarPat $ mkName "Nothing") (mkCaseRhs $ mkLit $ mkIntLit 0) Nothing
--          ])
--    , ("multiway if",   "if | x > y -> x\n"
--                     ++ "   | otherwise -> y"
--      , mkMultiIf [ mkGuardedCaseRhs
--                     [ mkGuardCheck $ mkInfixApp (mkVar (mkName "x")) (mkUnqualOp ">") (mkVar (mkName "y"))]
--                     (mkVar (mkName "x"))
--                  , mkGuardedCaseRhs [mkGuardCheck $ mkVar (mkName "otherwise")] (mkVar (mkName "y"))
--                  ])
--    , ("do notation",   "do x <- a\n"
--                     ++ "   return x"
--      , mkDoBlock [ G.mkBindStmt (mkVarPat (mkName "x")) (mkVar (mkName "a"))
--                  , mkExprStmt (mkApp (mkVar $ mkName "return") (mkVar $ mkName "x"))
--                  ])
--    ]
--
--testPatterns
--  = [ ("irrefutable pattern", "~[0, a]", mkIrrefutablePat $ mkListPat [ mkLitPat (mkIntLit 0), mkVarPat (mkName "a") ])
--    , ("named pattern", "p@Point{ x = 1 }"
--      , mkAsPat (mkName "p") $ mkRecPat (mkName "Point")
--                                 [ mkPatternField (mkName "x") (mkLitPat (mkIntLit 1)) ])
--    , ("bang pattern", "!(_, f -> 3)"
--      , mkBangPat $ mkTuplePat [mkWildPat, mkViewPat (mkVar $ mkName "f") (mkLitPat (mkIntLit 3))])
--    ]
--
--testType
--  = [ ("forall type", "forall x . Eq x => x -> ()"
--      , mkForallType [mkTypeVar (mkName "x")]
--          $ mkCtxType (mkContext (mkClassAssert (mkName "Eq") [mkVarType (mkName "x")]))
--          $ mkFunctionType (mkVarType (mkName "x")) (mkVarType (mkName "()")))
--    , ("type operators", "(A :+: B) (x, x)"
--      , mkTypeApp (mkParenType $ mkInfixTypeApp (mkVarType (mkName "A")) (mkUnqualOp ":+:") (mkVarType (mkName "B")))
--                  (mkTupleType [ mkVarType (mkName "x"), mkVarType (mkName "x") ]))
--    ]
--
--testBinds
--  = [ ("locals",   "x = (a, b) where a = 3\n"
--                ++ "                 b = 4"
--      , mkSimpleBind (mkVarPat (mkName "x")) (mkUnguardedRhs (mkTuple [(mkVar (mkName "a")), (mkVar (mkName "b"))]))
--                     (Just $ mkLocalBinds' [ mkLocalValBind $ mkSimpleBind' (mkName "a") (mkLit $ mkIntLit 3)
--                                           , mkLocalValBind $ mkSimpleBind' (mkName "b") (mkLit $ mkIntLit 4)
--                                           ]) )
--    , ("function bind",   "f i 0 = i\n"
--                       ++ "f i x = x"
--      , mkFunctionBind' (mkName "f") [ ([mkVarPat $ mkName "i", mkLitPat $ mkIntLit 0], mkVar $ mkName "i")
--                                     , ([mkVarPat $ mkName "i", mkVarPat $ mkName "x"], mkVar $ mkName "x")
--                                     ])
--    ]
--
--testDecls
--  = [ ("signature", "id :: a -> a"
--      , mkTypeSigDecl $ mkTypeSignature (mkName "id") (mkFunctionType (mkVarType (mkName "a")) (mkVarType (mkName "a"))))
--    , ("binding", "id x = x"
--      , mkValueBinding $ mkFunctionBind' (mkName "id") [([mkVarPat $ mkName "x"], mkVar $ mkName "x")])
--    , ("datatype definition", "data A a = A a deriving Show"
--      , mkDataDecl mkDataKeyword Nothing (mkDeclHeadApp (mkNameDeclHead (mkName "A")) (mkTypeVar (mkName "a")))
--          [mkConDecl (mkName "A") [mkVarType (mkName "a")]] [mkDeriving [mkInstanceHead (mkName "Show")]])
--    , ("record definition", "data A = A { x :: Int }"
--      , mkDataDecl mkDataKeyword Nothing (mkNameDeclHead (mkName "A"))
--          [mkRecordConDecl (mkName "A") [mkFieldDecl [mkName "x"] (mkVarType (mkName "Int"))]] [])
--    , ("typeclass definition",    "class A t => C t where f :: t\n"
--                               ++ "                       type T t :: *"
--      , mkClassDecl (Just $ mkContext (mkClassAssert (mkName "A") [mkVarType (mkName "t")]))
--                    (mkDeclHeadApp (mkNameDeclHead (mkName "C")) (mkTypeVar (mkName "t"))) []
--                    (Just $ mkClassBody [ mkClassElemSig $ mkTypeSignature (mkName "f") (mkVarType (mkName "t"))
--                                        , mkClassElemTypeFam (mkDeclHeadApp (mkNameDeclHead (mkName "T"))
--                                                                            (mkTypeVar (mkName "t")))
--                                                             (Just $ mkTypeFamilyKindSpec $ mkKindConstraint $ mkKindStar)
--                                        ])
--      )
--    , ( "instance definition", "instance C Int where f = 0"
--      , mkInstanceDecl Nothing (mkInstanceRule Nothing $ mkAppInstanceHead (mkInstanceHead $ mkName "C") (mkVarType (mkName "Int")))
--          (Just $ mkInstanceBody [mkInstanceBind $ mkSimpleBind' (mkName "f") (mkLit $ mkIntLit 0)]))
--    , ("fixity definition", "infixl 6 +", mkFixityDecl $ mkInfixL 6 (mkUnqualOp "+"))
--    ]
--
--testModules
--  = [ ("empty module", "", G.mkModule [] Nothing [] [])
--    , ("exports", "module Test(x, A(a), B(..)) where"
--      , G.mkModule [] (Just $ mkModuleHead (G.mkModuleName "Test") Nothing
--          (Just $ mkExportSpecs
--                    [ mkExportSpec $ mkIESpec (mkName "x") Nothing
--                    , mkExportSpec $ mkIESpec (mkName "A") (Just $ mkSubList [mkName "a"])
--                    , mkExportSpec $ mkIESpec (mkName "B") (Just mkSubAll)
--                    ])) [] [])
--    , ("imports",  "\nimport qualified A\n"
--                  ++ "import B as BB(x)\n"
--                  ++ "import B hiding (x)"
--    , G.mkModule [] Nothing
--        [ mkImportDecl False True False Nothing (G.mkModuleName "A") Nothing Nothing
--        , mkImportDecl False False False Nothing (G.mkModuleName "B") (Just $ G.mkModuleName "BB")
--            (Just $ mkImportSpecList [mkIESpec (mkName "x") Nothing])
--        , mkImportDecl False False False Nothing (G.mkModuleName "B") Nothing
--            (Just $ mkImportHidingList [mkIESpec (mkName "x") Nothing])
--        ] [])
--    ]

