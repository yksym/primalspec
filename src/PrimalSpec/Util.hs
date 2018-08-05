module PrimalSpec.Util
( askStdIn
, askIORef
, askStringIORef
) where

import Text.Read (readMaybe)
import Data.IORef

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

