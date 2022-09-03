{- | The Renamer has two responsibilities

        - Interning and disambiguating names. I.e. if the name "a" is used twice, these will be
          represented by distinct values. This might improve performance, although the
          benefit is probably not as large as one might expect, since the Renamer is also in charge of

        - Replacing variable names by DeBrujin indices
-}
module Lily.Rename (rename, RenameError) where

import Lily.Effect.Fresh
import Lily.Prelude
import Lily.Syntax

data RenameError = UnboundVar Text deriving (Show)

-- Variables store their DeBrujin level in the environment (1-indexed),
-- The DeBrujin index of variable `x` can then easily be calculated by the size of the environment
-- minus the level of `x`
data RenameEnv = RenameEnv
    { varLevels :: Map Text Lvl
    }

emptyRenameEnv :: RenameEnv
emptyRenameEnv = RenameEnv{varLevels = mempty}

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
renameExpr env (Var name) = case lookup name (varLevels env) of
    Nothing -> throwError (UnboundVar name)
    Just Lvl{ level, lvlName } -> do
        lvlName' <- case lvlName of
            Nothing -> error "No level name in renamer" -- TODO: This really shouldn't have to be partial
            Just name -> pure name
        let debrujin =
                Ix
                    { index = size (varLevels env) - level
                    , ixName = lvlName'
                    }
        pure (Var debrujin)
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

{- | Generates a fresh 'Name' for the given input and updates
 the rename environment accordingly
-}
newVar ::
    (Fresh () Unique :> es) =>
    Text ->
    RenameEnv ->
    Eff es (Name, RenameEnv)
newVar x env@RenameEnv{varLevels} = do
    u <- fresh ()
    let name = UnsafeMkName x u
    let level = size varLevels + 1
    let env' = env{varLevels = insert x Lvl { lvlName = Just name, level } varLevels}
    pure (name, env')
