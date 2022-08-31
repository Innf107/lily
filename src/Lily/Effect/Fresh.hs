module Lily.Effect.Fresh (Fresh (..), fresh, runFreshUnique) where

import Lily.Prelude

import Effectful.Dispatch.Dynamic

data Fresh input output :: Effect where
    Fresh :: input -> Fresh input output m output

type instance DispatchOf (Fresh input output) = Dynamic

fresh :: (Fresh input output :> es) => input -> Eff es output
fresh = send . Fresh

runFreshUnique :: (IOE :> es) => Eff (Fresh () Unique : es) a -> Eff es a
runFreshUnique = interpret \_ (Fresh _) -> liftIO newUnique
