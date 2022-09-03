module Lily.Syntax (
    SourceExpr (..),
    CoreExpr (..),
    Value (..),
    Closure (..),
    Name (..),
    Ix (..),
    Lvl (..),
    incLevel,
    Pass (..),
    XName,
    EvalEnv (..),
) where

import GHC.Show qualified as S
import Lily.Prelude

import Lily.Config qualified as Config

data Name = UnsafeMkName
    { getName :: Text
    , unique :: Unique
    }

{- | Equality on names is *entirely determined by the attached unique*.
 This means that we absolutely have to make sure, that different names
 can never have the same unique value.
-}
instance Eq Name where
    (UnsafeMkName _ u1) == (UnsafeMkName _ u2) = u1 == u2

instance Ord Name where
    compare (UnsafeMkName _ u1) (UnsafeMkName _ u2) = compare u1 u2

instance S.Show Name where
    show (UnsafeMkName x u) =
        if Config.verboseNames (Config.getConfig ())
            then toString (x <> "#" <> show (hashUnique u))
            else toString x

{- | DeBrujin indices

 These are used to represent nameless variables, by the distance to the binder that defines them.
 Concretely, this means that a named term like `λa. a` would be represented as `λ $0`
 and  'λa. λb. a + b' would be represented as `λλ $1 + $0`

 See the documentation for @Lvl@ for differences between these and levels
-}
data Ix = Ix
    { index :: Int
    , ixName :: Name
    }

{- | DeBrujin levels

 Just like DeBrujin *indices*, DeBrujin *levels* are a nameless representation for variables.
 Unlike indices, these are determined by the distance between the associated binder and the beginning of the expression.
 This means that @λa. a@ would still be @λ $0@ with levels, but @λx. λy. x + y@ would be @λλ $1 + $0@
-}
data Lvl = Lvl
    { level :: Int
    , lvlName :: Maybe Name
    }

incLevel :: Lvl -> Lvl
incLevel lvl@Lvl{level} = lvl{level = level + 1}

instance Eq Ix where
    (Ix i _) == (Ix j _) = i == j

instance S.Show Ix where
    show (Ix i name) = show name <> "@" <> show i

instance Eq Lvl where
    (Lvl l1 _) == (Lvl l2 _) = l1 == l2

instance S.Show Lvl where
    show (Lvl l (Just name)) = show name <> "~" <> show l
    show (Lvl l Nothing) = "~" <> show l

data Pass = Parsed | Renamed

type XName :: Pass -> Type
type family XName p where
    XName Parsed = Text
    XName Renamed = Name

type XVar :: Pass -> Type
type family XVar p where
    XVar Parsed = Text
    XVar Renamed = Ix

data SourceExpr (p :: Pass)
    = Var (XVar p) -- x
    | Let (XName p) (Maybe (SourceExpr p)) (SourceExpr p) (SourceExpr p) -- let x [: e₁] = e₂ in e₃
    | App (SourceExpr p) (SourceExpr p) -- e₁ e₂
    | Lambda (XName p) (Maybe (SourceExpr p)) (SourceExpr p) -- λx. e₂ | λ(x : e₁). e₂
    | Hole -- _
    | NamedHole (XName p) -- ?x
    | Arrow (SourceExpr p) (SourceExpr p) -- e₁ -> e₂
    | Pi (XName p) (SourceExpr p) (SourceExpr p) -- (x : e₁) -> e₂
    | Type -- Type

deriving instance Show (SourceExpr Parsed)
deriving instance Show (SourceExpr Renamed)

data CoreExpr
    = CVar Ix
    | CLet Name CoreExpr CoreExpr CoreExpr
    | CApp CoreExpr CoreExpr
    | CLambda Name (Maybe CoreExpr) CoreExpr
    | CHole
    | CNamedHole Name
    | CPi (Maybe Name) CoreExpr CoreExpr
    | CType
    deriving (Show)

-- | Values are results of evaluation and are used in type checking via Normalization by Evalutation (NBE)
data Value
    = VVar Lvl
    | VApp Value ~Value
    | VLambda Name Closure
    | VPi (Maybe Name) ~Value Closure
    | VType
    deriving (Show)

-- Defined here for now, since we need the environment to define closures
-- and we need closures to define values.
-- TODO: Maybe we could use .hs-boot files in the future?
data EvalEnv = EvalEnv
    { vars :: [Value]
    }
    deriving (Show)

data Closure = Closure EvalEnv ~CoreExpr deriving (Show)
