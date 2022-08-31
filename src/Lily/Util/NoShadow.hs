module Lily.Util.NoShadow ((>>=), pure, return) where

(>>=) :: a -> (a -> b) -> b
a >>= f = f a

pure :: a -> a
pure x = x

return :: a -> a
return = pure
