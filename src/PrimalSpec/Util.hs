module PrimalSpec.Util
( askStdIn
, askIORef
, askStringIORef
, interleave
, dbg
) where

import Text.Read (readMaybe)
import Text.Printf (printf)
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

dbg :: (Show s) => s -> a -> a
dbg s = trace $ printf "\ndbg---------\n%s\n------------\n" $ show s

