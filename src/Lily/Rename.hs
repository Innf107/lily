{- | The Renamer has two responsibilities

        - Interning and disambiguating names. I.e. if the name "a" is used twice, these will be
          represented by distinct values. This might improve performance, although the
          benefit is probably not as large as one might expect, since the Renamer is also in charge of

        - Replacing variable names by DeBruijn indices
-}
module Lily.Rename (rename, RenameError) where

import Lily.Effect.Fresh
import Lily.Prelude hiding (size)
import Lily.Syntax

data RenameError = UnboundVar Span Text deriving (Show)

-- Variables store their Debruijn level in the environment (1-indexed),
-- The Debruijn index of variable `x` can then easily be calculated by the size of the environment
-- minus the level of `x`
data RenameEnv = RenameEnv
    { varLevels :: Map Text Lvl
    , -- We need to store the size separately, since `Map` discards duplicates, but we have to pretend
      -- it doesn't to calculate Debruijn indices.
      size :: Int
    }

emptyRenameEnv :: RenameEnv
emptyRenameEnv = RenameEnv{varLevels = mempty, size = 0}

rename ::
    (Fresh () Unique :> es, Error RenameError :> es) =>
    SourceExpr Parsed ->
    Eff es (SourceExpr Renamed)
rename = renameExpr emptyRenameEnv

renameExpr ::
    (Fresh () Unique :> es, Error RenameError :> es) =>
    RenameEnv ->
    SourceExpr Parsed ->
    Eff es (SourceExpr Renamed)
renameExpr env (Var span name) = case lookup name (varLevels env) of
    Nothing -> case asPrimOp name of
        Just primop -> pure (Prim span primop)
        Nothing -> throwError (UnboundVar span name)
    Just Lvl{level, lvlName} -> do
        case lvlName of
            Nothing -> error "No level name in renamer" -- TODO: This really shouldn't have to be partial
            Just name -> do
                let debruijn =
                        Ix
                            { index = size env - level
                            , ixName = name
                            }
                pure (Var span debruijn)
renameExpr env (Let span x (mty) value rest) = do
    (x', envWithX) <- newVar x env
    mty' <- traverse (renameExpr env) mty

    -- Lets are non-recursive, so this uses @env@ instead of @envWithX@
    value' <- renameExpr env value

    -- The remaining expressions *can* access @x@.
    rest' <- renameExpr envWithX rest

    pure $ Let span x' mty' value' rest'
renameExpr env (Inductive span x args constrs body) = undefined
renameExpr env (App span e1 e2) = App span <$> renameExpr env e1 <*> renameExpr env e2
renameExpr env (Lambda span x mty body) = do
    (x', envWithX) <- newVar x env

    -- The type of @x@ cannot mention @x@ (I.e. expressions such as `λ(x : x). x` are disallowed).
    mty' <- traverse (renameExpr env) mty
    body' <- renameExpr envWithX body
    pure (Lambda span x' mty' body')
renameExpr _ (Hole span) = pure (Hole span)
renameExpr env (NamedHole span name) = do
    (name', _) <- newVar name env
    pure (NamedHole span name')
renameExpr env (Pi span mname xTy e) = do
    (x', envWithX) <- case mname of
        Just x -> first Just <$> newVar x env
        Nothing -> first (const Nothing) <$> newVar "internalNameYouShouldReallyNeverEverSeeThis" env

    -- Again, the type of @x@ cannot mention @x@.
    xTy' <- renameExpr env xTy
    -- But the body of the Π type can.
    e' <- renameExpr envWithX e
    pure (Pi span x' xTy' e')
renameExpr _ (Type span) = pure (Type span)

{- | Generates a fresh 'Name' for the given input and updates
 the rename environment accordingly
-}
newVar ::
    (Fresh () Unique :> es) =>
    Text ->
    RenameEnv ->
    Eff es (Name, RenameEnv)
newVar x env@RenameEnv{varLevels, size} = do
    u <- fresh ()
    let name = UnsafeMkName x u
    let level = size + 1
    let env' = env{varLevels = insert x Lvl{lvlName = Just name, level} varLevels, size = size + 1}
    pure (name, env')
