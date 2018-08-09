{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module PrimalSpec.Util
( askStdIn
, askIORef
, askStringIORef
, interleave
, dbg
, useDebugShow
, UseDebugShow
, NoDebugShow(NoDebugShow)
, StdOutDebugShow(StdOutDebugShow)
) where

import Text.Read (readMaybe)
import Text.Printf (printf)
import Data.Reflection (Given, give, given)
import Data.IORef
import Debug.Trace (trace)
import Data.List (transpose)

interleave :: [a] -> [a] -> [a]
interleave xs ys = concat $ transpose [xs, ys]

ask' :: (Read a) => IO (Maybe a) -> String -> (a -> Bool) -> IO a
ask' s q p = do
    putStrLn q
    ans <- s
    ans' <- maybe (putStrLn "Parse Failed! Try Again!!" >> askStdIn q p) return ans
    if p ans' then return ans'
             else do
                    putStrLn "Check Failed! Try Again!!"
                    askStdIn q p


askStdIn :: (Read a) => String -> (a -> Bool) -> IO a
askStdIn = ask' $ readMaybe <$> getLine

askIORef :: (Read a) => IORef a -> String -> (a -> Bool) -> IO a
askIORef ref = ask' $ Just <$> readIORef ref

askStringIORef :: (Read a) => IORef String -> String -> (a -> Bool) -> IO a
askStringIORef ref = ask' $ readMaybe <$> readIORef ref


class DebugShow d where
    dbgShow :: (Show s) => d -> s -> a -> a

data SomeDebugShow = forall s. DebugShow s => SomeDebugShow s

instance DebugShow SomeDebugShow where
    dbgShow (SomeDebugShow d) = dbgShow d

data NoDebugShow = NoDebugShow
data StdOutDebugShow = StdOutDebugShow

instance DebugShow NoDebugShow where
    dbgShow NoDebugShow _ = id

instance DebugShow StdOutDebugShow where
    dbgShow StdOutDebugShow s = trace $ printf "\ndbg---------\n%s\n------------\n" $ show s

type UseDebugShow = Given SomeDebugShow

dbg :: (UseDebugShow, Show b) => b -> a -> a
dbg = dbgShow (given :: SomeDebugShow)

useDebugShow :: DebugShow s => s -> (Given SomeDebugShow => r) -> r
useDebugShow = give . SomeDebugShow


