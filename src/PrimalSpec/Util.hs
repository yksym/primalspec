module PrimalSpec.Util
( askStdIn
, askIORef
, askStringIORef
, interleave
, void
--, Eval
--, eval
--, throwError, get, put
--, either2maybe
) where

import Text.Read (readMaybe)
import Data.IORef
import Data.List (transpose)

--import Data.Functor.Identity (Identity, runIdentity)
--import Control.Monad.State (StateT, runStateT)
--import Control.Monad.Except (ExceptT, runExceptT)
--import Control.Monad.State.Class
--import Control.Monad.Error.Class


interleave :: [a] -> [a] -> [a]
interleave xs ys = concat $ transpose [xs, ys]

void :: (Monad m) => m a -> m ()
void m = m >> return ()

ask' :: (Read a) => IO (Maybe a) -> String -> (a -> Bool) -> IO a
ask' s q p = do
    ans <- s
    case ans of
        Just ans' -> if p ans'
            then return ans'
            else putStrLn "Check Failed! Try Again!!" >> askStdIn q p
        Nothing -> putStrLn "Parse Failed! Try Again!!" >> askStdIn q p

askStdIn :: (Read a) => String -> (a -> Bool) -> IO a
askStdIn q p = do
    putStrLn q
    ask' (readMaybe <$> getLine) q p

askIORef :: (Read a) => IORef a -> String -> (a -> Bool) -> IO a
askIORef ref = ask' $ Just <$> readIORef ref

askStringIORef :: (Read a) => IORef String -> String -> (a -> Bool) -> IO a
askStringIORef ref = ask' $ readMaybe <$> readIORef ref

--either2maybe :: Either a b -> Maybe b
--either2maybe (Right x) = Just x
--either2maybe (Left _) = Nothing
--
--type Eval s e a = StateT s (ExceptT e Identity) a
--
--eval :: Eval s e a -> s -> Maybe (a, s)
--eval m x = either2maybe $ runIdentity $ runExceptT $ runStateT m x
--
--isSuccess :: Eval s e a -> Bool
--isSuccess m = eval


