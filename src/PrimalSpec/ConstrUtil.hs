module PrimalSpec.ConstrUtil
( argOf
, constrNameOf
, constrName
) where

import Data.Data (Data, toConstr, showConstr, gfoldl)
import Unsafe.Coerce (unsafeCoerce)

constrName :: (Data a) => a -> String
constrName = showConstr . toConstr

isSameConstr :: (Data a) => a -> a -> Bool
isSameConstr x y = constrName x == constrName y

constrNameOf :: (Data a) => (b -> a) -> String
constrNameOf = constrName . dummy

arg1 :: forall a t. Data t => t -> a
arg1 x = result where
    (result, _, _)   = gfoldl go (undefined, 0::Int,) x
    go (_, 0, c) f   = (unsafeCoerce f, 1, c f)
    go (res, i, c) f = (res, i + 1, c f)

dummy :: (Data a) => (b -> a) -> a
dummy cstr = cstr undefined

argOf :: (Data b) => b -> (a -> b) -> Maybe a
argOf b cstr = if isSameConstr b (dummy cstr)
                    then Just (arg1 b)
                    else Nothing

