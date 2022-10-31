module Lily.Lexer (Token (..), TokenClass (..), lex, LexError (..)) where

import Data.Char qualified as Char
import Data.Text qualified as Text
import Lily.Prelude

data LexError = InvalidChar Char deriving (Show, Eq)

data TokenClass
    = IDENT Text
    | LAMBDA
    | LET
    | INDUCTIVE
    | IN
    | QUESTIONMARK
    | EQUALS
    | DOT
    | LPAREN
    | RPAREN
    | ARROW
    | UNDERSCORE
    | COLON
    | PIPE
    | TYPE
    deriving (Show, Eq)

data Token = Token {tokenClass :: TokenClass, tokenSpan :: Span}

instance Spanned Token where
    spanOf Token{tokenSpan} = tokenSpan

data LexState = Default | InIdent [Char] | InLineComment

reserved :: Map Text TokenClass
reserved = [("let", LET), ("inductive", INDUCTIVE), ("in", IN), ("Type", TYPE)]

inc :: Span -> Span
inc span@UnsafeMkSpan{endCol} = span{endCol = endCol + 1}

incLine :: Span -> Span
incLine span@UnsafeMkSpan{endLine} = span{endLine = endLine + 1, endCol = 1}

resetStart :: Span -> Span
resetStart UnsafeMkSpan{sourceFile, endLine, endCol} = UnsafeMkSpan{sourceFile, startLine = endLine, startCol = endCol, endLine, endCol}

lex :: Error LexError :> es => FilePath -> Text -> Eff es [Token]
lex filePath = go (UnsafeMkSpan filePath 1 1 1 1) Default
  where
    go oldSpan Default input =
        let span = resetStart oldSpan
         in case Text.uncons input of
                Nothing -> pure []
                Just (c, rest) -> case c of
                    'λ' -> (Token LAMBDA (inc span) :) <$> go (inc span) Default rest
                    '?' -> (Token QUESTIONMARK (inc span) :) <$> go (inc span) Default rest
                    '=' -> (Token EQUALS (inc span) :) <$> go (inc span) Default rest
                    '.' -> (Token DOT (inc span) :) <$> go (inc span) Default rest
                    '(' -> (Token LPAREN (inc span) :) <$> go (inc span) Default rest
                    ')' -> (Token RPAREN (inc span) :) <$> go (inc span) Default rest
                    '-' | Just newRest <- Text.stripPrefix ">" rest -> (Token ARROW (inc (inc span)) :) <$> go (inc (inc span)) Default newRest
                    '-' | Just newRest <- Text.stripPrefix "-" rest -> go (inc (inc span)) InLineComment newRest
                    '_' -> (Token UNDERSCORE (inc span) :) <$> go (inc span) Default rest
                    ':' -> (Token COLON (inc span) :) <$> go (inc span) Default rest
                    '|' -> (Token PIPE (inc span) :) <$> go (inc span) Default rest
                    '\n' -> go (incLine span) Default rest
                    _ | Char.isSpace c -> go (inc span) Default rest
                    _ | Char.isPrint c -> go (inc span) (InIdent (one c)) rest
                    _ -> throwError (InvalidChar c)
    go span (InIdent ident) input = case Text.uncons input of
        Nothing -> pure [buildIdent span ident]
        Just (c, rest) -> case c of
            _ | Char.isPrint c && not (Char.isSpace c) && c `notElem` ("()λ." :: [Char]) -> go (inc span) (InIdent (c : ident)) rest
            _ -> (buildIdent (inc span) ident :) <$> go (inc span) Default input
      where
        buildIdent span str =
            let ident = toText (reverse str)
             in case lookup ident reserved of
                    Nothing -> Token (IDENT ident) span
                    Just x -> Token x span
    go span InLineComment input = case Text.uncons input of
        Nothing -> pure []
        Just (c, rest) -> case c of
            '\n' -> go (incLine span) Default rest
            _ -> go (inc span) InLineComment rest
