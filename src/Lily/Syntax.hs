{-# LANGUAGE UndecidableInstances #-}

module Lily.Syntax (
    SourceExpr (..),
    CoreExpr (..),
    Value (..),
    NeverEqual (..),
    Closure (..),
    Name (..),
    internalName,
    Ix (..),
    Lvl (..),
    incLevel,
    PrimOp (..),
    asPrimOp,
    Pass (..),
    XName,
    EvalEnv (..),
) where

import GHC.Show qualified as S
import Lily.Prelude

import Lily.Config qualified as Config

import System.IO.Unsafe qualified as Unsafe

data Name = UnsafeMkName
    { getName :: Text
    , unique :: Unique
    }

internalUnique :: Unique
internalUnique = Unsafe.unsafePerformIO newUnique
{-# NOINLINE internalUnique #-}

internalName :: Text -> Name
internalName name =
    UnsafeMkName
        { getName = name
        , unique = internalUnique
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

{- | DeBruijn indices

 These are used to represent nameless variables, by the distance to the binder that defines them.
 Concretely, this means that a named term like `λa. a` would be represented as `λ $0`
 and  'λa. λb. a + b' would be represented as `λλ $1 + $0`

 See the documentation for @Lvl@ for differences between these and levels
-}
data Ix = Ix
    { index :: Int
    , ixName :: Name
    }

{- | DeBruijn levels

 Just like DeBruijn *indices*, DeBruijn *levels* are a nameless representation for variables.
 Unlike indices, these are determined by the distance between the associated binder and the beginning of the expression.
 This means that @λa. a@ would still be @λ $0@ with levels, but @λx. λy. x + y@ would be @λλ $1 + $0@
-}
data Lvl = Lvl
    { level :: Int
    , lvlName :: Maybe Name
    }

incLevel :: Lvl -> Lvl
incLevel Lvl{level} = Lvl{level = level + 1, lvlName = Nothing}

instance Eq Ix where
    (Ix i _) == (Ix j _) = i == j

instance S.Show Ix where
    show (Ix i name) =
        let Config.Config{printDebruijn} = Config.getConfig ()
         in if printDebruijn
                then show name <> "@" <> show i
                else show name

instance Eq Lvl where
    (Lvl l1 _) == (Lvl l2 _) = l1 == l2

instance S.Show Lvl where
    show (Lvl l (Just name)) =
        let Config.Config{printDebruijn} = Config.getConfig ()
         in if printDebruijn
                then show name <> "~" <> show l
                else show name
    -- We print the DeBruijn level unconditionally if we don't have a name.
    -- What else would we even return here?
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

type XPrimOp :: Pass -> Type
type family XPrimOp p where
    XPrimOp Parsed = Void
    XPrimOp Renamed = PrimOp

type SourceTy p = SourceExpr p

data SourceExpr (p :: Pass)
    = Var Span (XVar p) -- x
    | Prim Span (XPrimOp p)
    | Let Span (XName p) (Maybe (SourceTy p)) (SourceExpr p) (SourceExpr p) -- let x [: τ] = e₁ in e₂
    | Inductive Span (XName p) [(XName p, SourceTy p)] [(XName p, SourceTy p)] (SourceExpr p) -- let inductive x (x : τ)* = [| x : τ]* in e
    | App Span (SourceExpr p) (SourceExpr p) -- e₁ e₂
    | Lambda Span (XName p) (Maybe (SourceExpr p)) (SourceExpr p) -- λx. e₂ | λ(x : e₁). e₂
    | Hole Span -- _
    | NamedHole Span (XName p) -- ?x
    | Pi Span (Maybe (XName p)) (SourceExpr p) (SourceExpr p) -- (x : e₁) -> e₂
    | Type Span -- Type

data CoreExpr
    = CVar Ix
    | CPrim PrimOp
    | CLet Name CoreExpr CoreExpr CoreExpr
    | CApp CoreExpr CoreExpr
    | CLambda Name (Maybe CoreExpr) CoreExpr
    | CHole
    | CNamedHole Name
    | CPi (Maybe Name) CoreExpr CoreExpr
    | CType
    deriving (Eq)

-- | Values are results of evaluation and are used in type checking via Normalization by Evalutation (NBE)
data Value
    = VVar Lvl
    | VPrimClosure PrimOp Int (DList Value)
    | --                    ^   ^ already applied
      --                    | missing arguments
      VApp Value ~Value
    | VLambda Name Closure
    | VPi (Maybe Name) ~Value Closure
    | VType
    deriving (Eq)

newtype NeverEqual a = NeverEqual {unNeverEqual :: a}
    deriving newtype (Show)

instance Eq (NeverEqual a) where
    _ == _ = False

data PrimOp
    = UnsafeFix
    | UnsafeCoerce
    deriving (Eq)

asPrimOp :: Text -> Maybe PrimOp
asPrimOp "unsafeFix#" = Just UnsafeFix
asPrimOp "unsafeCoerce#" = Just UnsafeCoerce
asPrimOp _ = Nothing

instance Show PrimOp where
    show UnsafeFix = "unsafeFix#"
    show UnsafeCoerce = "unsafeCoerce#"

-- Defined here for now, since we need the environment to define closures
-- and we need closures to define values.
-- TODO: Maybe we could use .hs-boot files in the future?
data EvalEnv = EvalEnv
    { vars :: [Value]
    }
    deriving (Show, Eq)

data Closure = Closure EvalEnv ~CoreExpr deriving (Show, Eq)

-- precedences
atomPrec, appPrec, piPrec, letPrec :: Int
atomPrec = 3
appPrec = 2
piPrec = 1
letPrec = 0

par :: Int -> Int -> S.ShowS -> S.ShowS
par p p' = S.showParen (p' < p)

class PrettyName a where
    prettyS :: a -> S.ShowS

instance PrettyName Name where
    prettyS = S.shows
instance PrettyName Text where
    prettyS x = (toString x <>)
instance PrettyName Ix where
    prettyS = S.shows

instance PrettyName Void where
    prettyS = \case {}

instance PrettyName PrimOp where
    prettyS x = S.shows x

showTyped :: (PrettyName n, Show a) => (n, a) -> S.ShowS
showTyped (x, ty) =  ("(" <>) . prettyS x . (" : " <>) . S.showsPrec piPrec ty

instance (PrettyName (XVar p), PrettyName (XName p), PrettyName (XPrimOp p)) => S.Show (SourceExpr p) where
    showsPrec _ (Var _ x) = prettyS x
    showsPrec _ (Prim _ p) = prettyS p
    showsPrec p (Let _ x Nothing body rest) =
        par p letPrec $
            ("let " <>) . prettyS x . (" = " <>) . S.showsPrec appPrec body
                <> ("in\n" <>)
                    . S.showsPrec letPrec rest
    showsPrec p (Inductive _ x args [] exp) =
        par p letPrec $  ("let inductive " <>) . prettyS x . (foldMap (((" "<>) .) . showTyped) args)  . (" in " <>) . (S.showsPrec letPrec exp)
    showsPrec p (Inductive _ x args constrs exp) =
        par p letPrec $  ("let inductive " <>) . prettyS x . (foldMap (((" "<>) .) . showTyped) args) . (" = " <>)
            . foldMap (\(con, ty) -> ("    | " <>) . prettyS con . (" : "<>) . S.showsPrec letPrec ty . ("\n" <>)) constrs
            . (" in\n" <>) . (S.showsPrec letPrec exp)
    showsPrec p (Let _ x (Just ty) body rest) =
        par p letPrec $
            ("let " <>) . prettyS x . (" : " <>) . S.showsPrec piPrec ty
                . ("\n    = " <>)
                . S.showsPrec letPrec body
                . ("\nin\n" <>)
                . S.showsPrec letPrec rest
    showsPrec p (App _ e1 e2) =
        par p appPrec $ S.showsPrec appPrec e1 . (" " <>) . S.showsPrec atomPrec e2
    showsPrec p (Lambda _ x Nothing body) =
        par p letPrec $ ("λ" <>) . prettyS x . (". " <>) . S.showsPrec letPrec body
    showsPrec p (Lambda _ x (Just ty) body) =
        par p letPrec $ ("λ(" <>) . prettyS x . (" : " <>) . S.showsPrec appPrec ty . ("). " <>) . S.showsPrec letPrec body
    showsPrec _ (Hole _) = ("_" <>)
    showsPrec _ (NamedHole _ name) = ("?" <>) . prettyS name
    showsPrec p (Pi _ Nothing e1 e2) = par p piPrec $ S.showsPrec appPrec e1 . (" -> " <>) . S.showsPrec piPrec e2
    showsPrec p (Pi _ (Just x) dom cod) =
        par p piPrec $
            ("(" <>) . prettyS x . (" : " <>) . S.showsPrec appPrec dom . (") -> " <>)
                . S.showsPrec letPrec cod
    showsPrec _ (Type _) = ("Type" <>)

instance Show CoreExpr where
    showsPrec _ (CVar x) = S.shows x
    showsPrec _ (CPrim x) = S.shows x
    showsPrec p (CLet x ty body rest) =
        par p letPrec $
            ("let " <>) . prettyS x . (" : " <>) . S.showsPrec piPrec ty
                . ("\n    = " <>)
                . S.showsPrec letPrec body
                . ("\nin\n" <>)
                . S.showsPrec letPrec rest
    showsPrec p (CApp e1 e2) =
        par p appPrec $ S.showsPrec appPrec e1 . (" " <>) . S.showsPrec atomPrec e2
    showsPrec p (CLambda x Nothing body) =
        par p letPrec $ ("λ" <>) . prettyS x . (". " <>) . S.showsPrec letPrec body
    showsPrec p (CLambda x (Just ty) body) =
        par p letPrec $ ("λ(" <>) . prettyS x . (" : " <>) . S.showsPrec appPrec ty . ("). " <>) . S.showsPrec letPrec body
    showsPrec _ CHole = ("_" <>)
    showsPrec _ (CNamedHole name) = ("?" <>) . prettyS name
    showsPrec p (CPi (Just x) dom cod) =
        par p piPrec $
            ("(" <>) . prettyS x . (" : " <>) . S.showsPrec appPrec dom . (") -> " <>)
                . S.showsPrec letPrec cod
    showsPrec p (CPi Nothing dom cod) =
        par p piPrec $ S.showsPrec appPrec dom . (" -> " <>) . S.showsPrec piPrec cod
    showsPrec _ CType = ("Type" <>)

instance Show Value where
    showsPrec _ (VVar lvl) = S.shows lvl
    showsPrec _ (VPrimClosure prim _ []) = S.shows prim
    showsPrec p (VPrimClosure prim _ args) =
        par p appPrec $ S.shows prim . (foldMap (\x -> (" " <>) . S.showsPrec atomPrec x) args)
    showsPrec p (VApp v1 v2) =
        par p appPrec $ S.showsPrec appPrec v1 . (" " <>) . S.showsPrec atomPrec v2
    showsPrec p (VLambda x (Closure env rest)) =
        let Config.Config{printClosures} = Config.getConfig ()
         in par p letPrec $
                ("λ" <>) . S.shows x
                    . ( if printClosures
                            then ("[" <>) . (toString (intercalate ", " (map show (vars env))) <>) . ("]. " <>)
                            else (". " <>)
                      )
                    . S.showsPrec letPrec rest
    showsPrec p (VPi (Just x) dom (Closure env rest)) =
        let Config.Config{printClosures} = Config.getConfig ()
         in par p piPrec $
                ("(" <>) . prettyS x . (" : " <>) . S.showsPrec appPrec dom
                    . ( if printClosures
                            then (")[" <>) . (toString (intercalate ", " (map show (vars env))) <>) . ("] -> " <>)
                            else (") -> " <>)
                      )
                    . S.showsPrec letPrec rest
    showsPrec p (VPi Nothing dom (Closure env rest)) =
        let Config.Config{printClosures} = Config.getConfig ()
         in par p piPrec $
                S.showsPrec appPrec dom
                    . ( if printClosures
                            then ("[" <>) . (toString (intercalate ", " (map show (vars env))) <>) . ("] -> " <>)
                            else (" -> " <>)
                      )
                    . S.showsPrec piPrec rest
    showsPrec _ (VType) = ("Type" <>)

instance Spanned (SourceExpr p) where
    spanOf = \case
        Var span _ -> span
        Prim span _ -> span
        Let span _ _ _ _ -> span
        Inductive span _ _ _ _ -> span
        App span _ _ -> span
        Lambda span _ _ _ -> span
        Hole span -> span
        NamedHole span _ -> span
        Pi span _ _ _ -> span
        Type span -> span
