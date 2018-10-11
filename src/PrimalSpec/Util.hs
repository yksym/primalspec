module PrimalSpec.Util
( askStdIn
, interleave
, void
, constrName
) where

import Text.Read (readMaybe)
import Data.List (transpose)
import Data.Data (Data, toConstr, showConstr)


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

constrName :: (Data a) => a -> String
constrName = showConstr . toConstr

