{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import PrimalSpec
--import Control.Lens
import Control.Monad(unless)
import Options.Generic
import Data.Maybe (fromMaybe)

data OptArgs = OptArgs String (Maybe Int) deriving (Generic, Show)

instance ParseRecord OptArgs

num2enums :: Int -> [LogLv]
num2enums n = [toEnum i | (i,k) <- zip [1..] $ i2b n, k == 1]

i2b :: Int -> [Int]
i2b 0 = []
i2b m = mod m 2 : i2b (div m 2)

main :: IO ()
main = do
    (OptArgs filepath l) <- getRecord "Test program"
    s <- readFile filepath
    let ls = num2enums $ fromMaybe 0 l
    unless (null ls) $ print ls
    setLogLevel ls
    case runParser s of
        Right p -> putStrLn $ checkAllAssert p
        Left s' -> print s'
