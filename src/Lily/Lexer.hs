module Lily.Lexer (Token (..), lex, LexError (..)) where

import Data.Char qualified as Char
import Data.Text qualified as Text
import Lily.Prelude

data LexError = InvalidChar Char deriving (Show, Eq)

data Token
    = IDENT Text
    | LAMBDA
    | LET
    | IN
    | QUESTIONMARK
    | EQUALS
    | DOT
    | LPAREN
    | RPAREN
    | ARROW
    | UNDERSCORE
    | COLON
    | TYPE
    deriving (Show, Eq)

data LexState = Default | InIdent [Char] | InLineComment

lex :: Error LexError :> es => Text -> Eff es [Token]
lex = go Default
  where
    go Default input = case Text.uncons input of
        Nothing -> pure []
        Just (c, rest) -> case c of
            'λ' -> (LAMBDA :) <$> go Default rest
            'l' | Just newRest <- Text.stripPrefix "et" rest -> (LET :) <$> go Default newRest
            'i' | Just newRest <- Text.stripPrefix "n" rest -> (IN :) <$> go Default newRest
            '?' -> (QUESTIONMARK :) <$> go Default rest
            '=' -> (EQUALS :) <$> go Default rest
            '.' -> (DOT :) <$> go Default rest
            '(' -> (LPAREN :) <$> go Default rest
            ')' -> (RPAREN :) <$> go Default rest
            '-' | Just newRest <- Text.stripPrefix ">" rest -> (ARROW :) <$> go Default newRest
            '-' | Just newRest <- Text.stripPrefix "-" rest -> go InLineComment newRest
            '_' -> (UNDERSCORE :) <$> go Default rest
            ':' -> (COLON :) <$> go Default rest
            'T' | Just newRest <- Text.stripPrefix "ype" rest -> (TYPE :) <$> go Default newRest
            _ | Char.isAlpha c -> go (InIdent (one c)) rest
            _ | Char.isSpace c -> go Default rest
            _ -> throwError (InvalidChar c)
    go (InIdent ident) input = case Text.uncons input of
        Nothing -> pure [IDENT (toText (reverse ident))]
        Just (c, rest) -> case c of
            _ | Char.isAlphaNum c || c `elem` ("_-'" :: String) -> go (InIdent (c : ident)) rest
            _ -> (IDENT (toText (reverse ident)) :) <$> go Default input
    go InLineComment input = case Text.uncons input of
        Nothing -> pure []
        Just (c, rest) -> case c of
            '\n' -> go Default rest
            _    -> go InLineComment rest

