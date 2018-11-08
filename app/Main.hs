{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import PrimalSpec
--import Control.Lens
import Options.Generic
import Data.Maybe (fromMaybe)

data OptArgs = OptArgs String (Maybe Int) deriving (Generic, Show)

instance ParseRecord OptArgs

main :: IO ()
main = do
    (OptArgs filepath logLv) <- getRecord "Test program"
    s <- readFile filepath
    let l = toEnum $ fromMaybe 0 logLv
    setLogLevel l
    case runParser s of
        Right p -> do
            putStrLn $ checkAllAssert p
        Left s' -> print s'
