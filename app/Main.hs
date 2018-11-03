{-# LANGUAGE OverloadedStrings #-}
module Main where

import PrimalSpec
--import Control.Lens
import System.Environment (getArgs)



main :: IO ()
main = do
    args <- getArgs
    s <- readFile $ head args
    case runParser s of
        Right p -> do
            --sequence_ $ print <$> (p ^. tyCtx)
            --sequence_ $ print <$> (p ^. vCtx)
            putStrLn $ checkAllAssert p
        Left s' -> print s'





