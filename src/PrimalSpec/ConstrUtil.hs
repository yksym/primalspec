module PrimalSpec.ConstrUtil
( argOf
, constrNameOf
, constrName
) where

import Data.Data (Data, toConstr, showConstr, gfoldl)
import Unsafe.Coerce (unsafeCoerce)
import Data.Default

constrName :: (Data a) => a -> String
constrName = showConstr . toConstr

isSameConstr :: (Data a) => a -> a -> Bool
isSameConstr x y = constrName x == constrName y

constrNameOf :: (Data a, Default b) => (b -> a) -> String
constrNameOf = constrName . dummy

arg1 :: forall b a. (Data a, Default b) => a -> b
arg1 x = result where
    (result, _, _)   = gfoldl go (def, 0::Int,) x
    go (_, 0, c) f   = (unsafeCoerce f, 1, c f)
    go (res, i, c) f = (res, i + 1, c f)

dummy :: (Data a, Default b) => (b -> a) -> a
dummy cstr = cstr def

argOf :: (Data a, Default b) => a -> (b -> a) -> Maybe b
argOf a cstr = if isSameConstr a (dummy cstr)
                    then Just (arg1 a)
                    else Nothing

