module Lily.Types (TypeError (..)) where

import Lily.Prelude
import Lily.Syntax

data TypeError = UnableToMatch (Expr Renamed) (Expr Renamed) 
                --              ^ expected     ^ actual
               | AmbiguousLambdaArgument Name
               deriving (Show)

{- | The type checking environment.

 Since every name has been renamed in the renamer already, we don't need to fight with scopes
 and can simply thread the environment along in a @State@ effect, instead of passing it
 in a @Reader@.

 This has the downside of being less memory efficient, since we never drop entries, but
 it does make things easier.
-}
newtype TCEnv = TCEnv
    { varTypes :: Map Name (Expr Renamed)
    } deriving (Show)

insertVar :: (State TCEnv :> es) => Name -> Expr Renamed -> Eff es ()
insertVar x ty = modify \env@TCEnv{varTypes} -> env{varTypes = insert x ty varTypes}

check ::
    (State TCEnv :> es, Error TypeError :> es) =>
    Expr Renamed ->
    Expr Renamed ->
    Eff es ()
check expr expectedTy = case expr of
    Var x -> undefined
    Let xn m_ex ex ex' -> undefined
    App ex ex' -> undefined
    Lambda xn m_ex ex -> undefined
    Hole -> undefined
    NamedHole xn -> undefined
    Arrow ex ex' -> undefined
    Pi xn ex ex' -> undefined
    Type -> undefined

infer ::
    (State TCEnv :> es, Error TypeError :> es) =>
    Expr Renamed ->
    Eff es (Expr Renamed)
infer expr = case expr of
    Var x -> do
        gets (lookup x . varTypes) >>= \case
            Nothing -> error ("Variable not found during type check: " <> show x)
            Just ty -> pure ty
    Let x mty body rest -> do
        xTy <- case mty of
            Nothing -> infer body
            Just ty -> check body ty >> pure ty
        insertVar x xTy
        infer rest
    App ex ex' -> undefined
    Lambda x (Just xTy) body -> do
        insertVar x xTy
        resTy <- infer body
        -- How do we infer Π types?
        undefined
    -- We cannot infer @x@ without unification variables or something similar. 
    -- Maybe we could use meta variables here somehow? Who knows.
    Lambda x Nothing _ -> throwError (AmbiguousLambdaArgument x)
    Hole -> undefined
    NamedHole xn -> undefined
    Arrow ex ex' -> undefined
    Pi xn ex ex' -> undefined
    -- Yes, TypeInType. Fight me
    Type -> pure Type

