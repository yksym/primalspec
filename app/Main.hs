{-# LANGUAGE OverloadedStrings #-}

module Main where

--import PrimalSpec
import PrimalSpec.Lexer
import PrimalSpec.Parser
import qualified Data.ByteString.Lazy.Char8 as B

convStmt (ProcDef proc exp) = mkAssign $ (convProc proc) (convExp exp)
convStmt (RefTDef proc exp) = undefined

convProgram :: Program -> Either String String
convProgram (Program hd et st is ep stmts ft) = do
    return $ convertStmt <$> stmts
    -- return hd ++ ft

main :: IO ()
main = do
    s <- getContents
    let p = runAlex (B.pack s) prspParser
    print $ convProgram p









--let x = runAlex (B.pack s) scan
--print $ x
--scan = go []
--    where
--    go xs = do
--        (Token _ x) <- alexMonadScan
--        if x == TokenEOF then return xs
--                         else go $ xs ++ [x]

