{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import PrimalSpec
--import Control.Lens
--import Control.Monad.IO.Class
import Options.Generic
import Data.Maybe (fromMaybe)

data OptArgs = OptArgs String (Maybe Int) deriving (Generic, Show)

instance ParseRecord OptArgs

main :: IO ()
main = do
    (OptArgs filepath logLv) <- getRecord "Test program"
    s <- readFile filepath
    setLogLevel $ toEnum $ fromMaybe 0 logLv
    case runParser s of
        Right p -> putStrLn $ checkAllAssert p
        Left s' -> print s'
