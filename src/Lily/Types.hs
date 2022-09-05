module Lily.Types (
    TypeError (..),
    infer,
    check,
    eval,
    TCEnv,
    emptyTCEnv,
    emptyEvalEnv,
) where

import Lily.Prelude
import Lily.Syntax

import Lily.Config as Config

data TypeError
    = ConversionError Value Value Value Value
    | --              ^     ^     ^     ^ full actual
      --              |     |     | full expected
      --              |     | actual
      --              | expected
      AmbiguousLambdaArgument Name
    | AmbiguousHoles
    | NonFunctionApplication Value (SourceExpr Renamed)
    deriving (Show)

data TCEnv = TCEnv
    { varTypes :: [Value]
    , evalEnv :: EvalEnv
    , currentLevel :: Lvl
    }
    deriving (Show)

emptyTCEnv :: TCEnv
emptyTCEnv =
    TCEnv
        { varTypes = []
        , evalEnv = emptyEvalEnv
        , currentLevel =
            Lvl
                { level = 0
                , lvlName = Nothing
                }
        }

emptyEvalEnv :: EvalEnv
emptyEvalEnv =
    EvalEnv
        { vars = []
        }

-- | Add a variable with a given value and type to the typing environment
addDefinition :: Value -> Value -> TCEnv -> TCEnv
addDefinition value ty env@TCEnv{varTypes, evalEnv, currentLevel} =
    env
        { varTypes = ty : varTypes
        , evalEnv = insertValue value evalEnv
        , currentLevel = incLevel currentLevel
        }

{- | Add a local parameter with a currently unknown definition but known type to the
 typing environment.
 This variant is used in e.g. λ- or Π-expressions, but not in let-bindings where
 we know the value we are binding already
-}
addParamDefinition :: Maybe Name -> Value -> TCEnv -> TCEnv
addParamDefinition mname ty env@TCEnv{currentLevel} =
    addDefinition (VVar (currentLevel{lvlName = mname})) ty env

lookupType :: HasCallStack => TCEnv -> Ix -> Value
lookupType TCEnv{varTypes} Ix{index, ixName} = go varTypes index
  where
    go [] _ = error $ "Unbound variable (this should have been caught earlier): '" <> show ixName <> "'"
    go (ty : _) 0 = ty
    go (_ : tys) i = go tys (i - 1)

insertValue :: Value -> EvalEnv -> EvalEnv
insertValue val env@EvalEnv{vars} = env{vars = val : vars}

lookupValue :: HasCallStack => EvalEnv -> Ix -> Value
lookupValue EvalEnv{vars} Ix{index, ixName} = go vars index
  where
    go [] _ = error $ "Unbound variable (this should have been caught earlier): '" <> show ixName <> "'"
    go (v : _) 0 = v
    go (_ : vs) i = go vs (i - 1)

check ::
    (Error TypeError :> es) =>
    TCEnv ->
    SourceExpr Renamed ->
    Value ->
    Eff es CoreExpr
check _ expr ty | False <- trace TC ("[check]: (" <> show expr <> ") : " <> show ty) True = error "unreachable"
-- We can ignore the name in the Π type, since we only include it for diagnostics anyway.
-- The variable is represented by its DeBrujin index.
check env (Lambda x Nothing body) (VPi _ dom closure) = do
    CLambda x Nothing
        -- We again don't need to insert the name, since Core already uses DeBrujin indices
        <$> check (addParamDefinition (Just x) dom env) body (applyClosure closure (VVar ((currentLevel env){lvlName = Just x})))
check env (Let x mty body rest) ty = do
    (ty', body', bodyTy) <- inferLet env mty body

    let ~bodyValue = eval (evalEnv env) body'

    rest' <- check (addDefinition bodyValue bodyTy env) rest ty
    pure (CLet x ty' body' rest')
check env expr expectedTy = do
    (expr', inferredTy) <- infer env expr
    conversionCheck (currentLevel env) inferredTy expectedTy
    pure expr'

infer ::
    (Error TypeError :> es) =>
    TCEnv ->
    SourceExpr Renamed ->
    Eff es (CoreExpr, Value)
infer env expr = do
    traceM TC ("[infer]: " <> show expr)
    case expr of
        Var x -> pure (CVar x, lookupType env x)
        Let x mty body rest -> do
            (ty', body', bodyTy) <- inferLet env mty body

            -- Not a massive fan of using implicit laziness here but okay
            let ~bodyValue = eval (evalEnv env) body'

            (rest', restTy) <- infer (addDefinition bodyValue bodyTy env) rest
            pure (CLet x ty' body' rest', restTy)
        e@(App fun arg) -> do
            (fun', funTy) <- infer env fun
            case funTy of
                VPi _ dom clos -> do
                    arg' <- check env arg dom
                    pure (CApp fun' arg', applyClosure clos (eval (evalEnv env) arg'))
                _ -> throwError (NonFunctionApplication funTy e)
        Lambda x (Just tySourceExpr) body -> do
            tyCoreExpr <- check env tySourceExpr VType -- Same reasoning as type annotations in 'let' above
            let ty = eval (evalEnv env) tyCoreExpr
            let env' = addParamDefinition (Just x) ty env
            (body', bodyTy) <- infer env' body

            -- TODO: I *think* the closure should include the unmodified `env`,
            -- since the type of `x` is given by the Π type, but I'm not entirely sure.
            -- Also, I think `bodyTy` should be quoted in env', since it includes `x`?
            let resultTy = VPi (Just x) ty (Closure (evalEnv env) (quote (currentLevel env') bodyTy))

            pure (CLambda x (Just tyCoreExpr) body', resultTy)
        -- We cannot infer @x@ without unification variables or something similar.
        -- Maybe we could use meta variables here somehow? Who knows.
        Lambda x Nothing _ -> throwError (AmbiguousLambdaArgument x)
        Hole -> undefined
        NamedHole xn -> undefined
        Pi x dom cod -> do
            dom' <- check env dom VType
            cod' <- check (addParamDefinition x (eval (evalEnv env) dom') env) cod VType
            pure (CPi x dom' cod', VType)
        -- Yes, TypeInType. Fight me
        Type -> pure (CType, VType)

inferLet ::
    Error TypeError :> es =>
    TCEnv ->
    Maybe (SourceExpr Renamed) ->
    SourceExpr Renamed ->
    Eff es (CoreExpr, CoreExpr, Value)
inferLet env mty body = case mty of
    Nothing -> do
        (body', bodyTy) <- infer env body
        -- For unannotated lambdas, we insert an annotation by quoting
        -- the inferred type. This might be unnecessary and it might be
        -- more intelligent to drop the type annotation for core entirely?
        -- Or maybe we should keep the Maybe?
        -- I'm not sure.
        pure (quote (currentLevel env) bodyTy, body', bodyTy)
    Just tySourceExpr -> do
        tyCoreExpr <- check env tySourceExpr VType -- Type annotations have to be, well... types! (We also need to do this to get a CoreExpr for tySourceExpr)
        let bodyTy = eval (evalEnv env) tyCoreExpr
        traceM TC ("[inferLet] tySourceExpr : " <> show tySourceExpr <> " | tyCoreExpr: " <> show tyCoreExpr <> " | bodyTy: " <> show bodyTy)
        body' <- check env body bodyTy
        pure (tyCoreExpr, body', bodyTy)

conversionCheck :: Error TypeError :> es => Lvl -> Value -> Value -> Eff es ()
conversionCheck level expected inferred = case (expected, inferred) of
    (VType, VType) -> pure ()
    (VPi x1 dom1 clos1, VPi x2 dom2 clos2) -> do
        conversionCheck level dom1 dom2
        -- We check that the codomains are equivalent by
        -- applying them to a 'free'(?) variable.
        conversionCheck
            (incLevel level)
            (applyClosure clos1 (VVar (level{lvlName = x1})))
            (applyClosure clos2 (VVar (level{lvlName = x2})))
    (VLambda x1 clos1, VLambda x2 clos2) ->
        -- Same reasoning as Π types, except we don't have a domain to check.
        conversionCheck
            (incLevel level)
            (applyClosure clos1 (VVar (level{lvlName = Just x1})))
            (applyClosure clos2 (VVar (level{lvlName = Just x2})))
    (VLambda x clos1, value) ->
        -- In this case, the second argument is not known to be a lambda yet
        -- (It's probably either free variable or another application involving free variables),
        -- So we do the same check as in the case above, but create a new `VApp` in the second case
        -- insetead of applying a closure (which we don't have yet!)
        conversionCheck
            (incLevel level)
            (applyClosure clos1 (VVar (level{lvlName = Just x})))
            (VApp value (VVar level))
    -- Thanks to DeBrujin indices, we don't need to deal with
    -- α-conversion or anything annyoing like that here
    (VVar lvl1, VVar lvl2) | lvl1 == lvl2 -> pure ()
    (VApp fun1 arg1, VApp fun2 arg2) -> do
        conversionCheck level fun1 fun2
        conversionCheck level arg1 arg2
    (ty1, ty2) -> throwError (ConversionError ty1 ty2 expected inferred)

eval :: HasCallStack => EvalEnv -> CoreExpr -> Value
eval _ expr | False <- trace Config.Eval ("[eval]: " <> show expr) True = error "unreachable"
eval env (CVar ix) =
    let result = lookupValue env ix
     in trace Config.Eval ("[eval var]: " <> show ix <> " ==> " <> show result) result
eval env (CLet x _ body rest) =
    eval (insertValue (eval env body) env) rest
eval env (CApp e1 e2) = case (eval env e1, eval env e2) of
    (VLambda _ clos, v2) -> applyClosure clos v2
    (v1, v2) -> VApp v1 v2
-- Evaluating a lambda drops its type signature.
-- TODO: We could probably already drop the signature
-- during evaluation. I don't think we need it in Core.
eval env (CLambda x _ body) =
    VLambda x (Closure env body)
eval _env CHole = undefined -- I think this should error? Eval can't be lazy unless its pure though so... not sure what to do here for now.
eval _env CNamedHole{} = undefined
eval env (CPi name dom cod) =
    VPi name (eval env dom) (Closure env cod)
eval _ CType = VType

applyClosure :: Closure -> Value -> Value
applyClosure (Closure env expr) val = eval (insertValue val env) expr

quote :: Lvl -> Value -> CoreExpr
quote = undefined
