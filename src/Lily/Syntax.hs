module Lily.Syntax (Expr (..), Name (..), Pass (..), XName) where

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
        if Config.verboseNames (Config.getConfig ()) then
            toString (x <> "@" <> show (hashUnique u))
        else
            toString x

data Pass = Parsed | Renamed

type XName :: Pass -> Type
type family XName p where
    XName Parsed = Text
    XName Renamed = Name

data Expr (p :: Pass)
    = Var (XName p) -- x
    | Let (XName p) (Maybe (Expr p)) (Expr p) (Expr p) -- let x [: e₁] = e₂ in e₃
    | App (Expr p) (Expr p) -- e₁ e₂
    | Lambda (XName p) (Maybe (Expr p)) (Expr p) -- λx. e₂ | λ(x : e₁). e₂
    | Hole -- _
    | NamedHole (XName p) -- ?x
    | Arrow (Expr p) (Expr p) -- e₁ -> e₂
    | Pi (XName p) (Expr p) (Expr p) -- (x : e₁) -> e₂
    | Type -- Type

deriving instance Show (Expr Parsed)
deriving instance Show (Expr Renamed)
