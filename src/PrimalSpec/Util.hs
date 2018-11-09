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

data LogLv = NO_LOG | EVENT_TRACE | TRACE_CTX | JUDGE | DEBUG deriving(Ord,Eq,Enum,Read,Show)

setLogLevel :: [LogLv] -> IO ()
setLogLevel ls = setEnv "DEBUG" $ show $ ls

readEnv :: String -> IO [LogLv]
readEnv k = getEnv k >>= \n -> return $ read n

dlog :: LogLv -> String -> b -> b
dlog l x y = unsafePerformIO $ do -- if x is constant, H may eval it only once...orz
    ls <- readEnv "DEBUG"
    --print (l, ls)
    when (elem l ls) $ putStrLn x
    return y

dlogM :: (Monad m) => LogLv -> String -> m ()
dlogM l x = dlog l x $ return ()

