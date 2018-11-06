{-# LANGUAGE ExistentialQuantification, ConstraintKinds, FlexibleContexts, RankNTypes #-}

module PrimalSpec.Util
( dlog
, dlogM
, setLogLevel
, LogLv(..)
) where

import System.IO.Unsafe (unsafePerformIO)
import Control.Monad(when)
import System.Environment

data LogLv = NO_LOG | EVENT_TRACE | DEBUG deriving(Ord,Eq,Enum,Read,Show)

setLogLevel :: LogLv -> IO ()
setLogLevel lv = setEnv "DEBUG" $ show $ fromEnum lv

readEnv :: String -> IO Int
readEnv k = getEnv k >>= \n -> return $ read n

dlog :: LogLv -> String -> b -> b
dlog l x y = unsafePerformIO $ do -- if x is constant, H may eval it only once...orz
    l' <- toEnum <$> readEnv "DEBUG"
    when (l == l') $ putStrLn x
    return y

dlogM :: (Monad m) => LogLv -> String -> m ()
dlogM l x = dlog l x $ return ()

