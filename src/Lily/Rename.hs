module Lily.Rename (rename, RenameError) where

import Lily.Effect.Fresh
import Lily.Prelude
import Lily.Syntax

data RenameError = UnboundVar Text deriving (Show)

data RenameEnv = RenameEnv
    { vars :: Map Text Name
    }

emptyRenameEnv :: RenameEnv
emptyRenameEnv = RenameEnv{vars = mempty}

rename ::
    (Fresh () Unique :> es, Error RenameError :> es) =>
    Expr Parsed ->
    Eff es (Expr Renamed)
rename = renameExpr emptyRenameEnv

renameExpr ::
    (Fresh () Unique :> es, Error RenameError :> es) =>
    RenameEnv ->
    Expr Parsed ->
    Eff es (Expr Renamed)
renameExpr env (Var name) = case lookup name (vars env) of
    Nothing -> throwError (UnboundVar name)
    Just renamed -> pure (Var renamed)
renameExpr env (Let x (mty) value rest) = do
    (x', envWithX) <- newVar x env
    mty' <- traverse (renameExpr env) mty

    -- Lets are non-recursive, so this uses @env@ instead of @envWithX@
    value' <- renameExpr env value

    -- The remaining expressions *can* access @x@.
    rest' <- renameExpr envWithX rest

    pure $ Let x' mty' value' rest'
renameExpr env (App e1 e2) = App <$> renameExpr env e1 <*> renameExpr env e2
renameExpr env (Lambda x mty body) = do
    (x', envWithX) <- newVar x env

    -- The type of @x@ cannot mention @x@ (I.e. expressions such as `λ(x : x). x` are disallowed).
    mty' <- traverse (renameExpr env) mty
    body' <- renameExpr envWithX body
    pure (Lambda x' mty' body')
renameExpr _ Hole = pure Hole
renameExpr env (NamedHole name) = do
    (name', _) <- newVar name env
    pure (NamedHole name')
renameExpr env (Arrow e1 e2) =
    Arrow <$> renameExpr env e1 <*> renameExpr env e2
renameExpr env (Pi x xTy e) = do
    (x', envWithX) <- newVar x env
    -- Again, the type of @x@ cannot mention @x@.
    xTy' <- renameExpr env xTy
    -- But the body of the Π type can.
    e' <- renameExpr envWithX e
    pure (Pi x' xTy' e')
renameExpr _ Type = pure Type

newVar ::
    (Fresh () Unique :> es) =>
    Text ->
    RenameEnv ->
    Eff es (Name, RenameEnv)
newVar x env@RenameEnv{vars} = do
    u <- fresh ()
    let name = UnsafeMkName x u
    let env' = env{vars = insert x name vars}
    pure (name, env')
