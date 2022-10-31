{
{-# LANGUAGE NoStrictData #-}
module Lily.Parser (parse) where

import Prelude

import Relude (Text)

import Lily.Syntax (SourceExpr(..), Pass(..), Name)
import Lily.Lexer (Token(..), TokenClass(..))

import Lily.Parser.Util
}

%name parse Expr
%tokentype { Token }
%error { parseError }

%token identS           { (IdentToken $$) }
%token 'λ'              { (Token LAMBDA _) }
%token let              { (Token LET _) }
%token inductive        { (Token INDUCTIVE _) }
%token in               { (Token IN _) }
%token '?'              { (Token QUESTIONMARK _) }
%token '='              { (Token EQUALS _) }
%token '.'              { (Token DOT _) }
%token '('              { (Token LPAREN _) }
%token ')'              { (Token RPAREN _) }
%token '->'             { (Token ARROW _) }
%token '_'              { (Token UNDERSCORE _) }
%token ':'              { (Token COLON _) }
%token '|'              { (Token PIPE _) }
%token type             { (Token TYPE _) }

%%

-- Defined for convenience to allow omitting unnecessary spans from indentifiers  
ident :: { Text }
ident : identS { fst $1 }

Expr :: { SourceExpr Parsed }
Expr : let ident '=' Expr in Expr                                       { Let (withSpan $1 $6) $2 Nothing $4 $6 }
     | let ident ':' Expr '=' Expr in Expr                              { Let (withSpan $1 $8) $2 (Just $4) $6 $8 }
     | let inductive ident TypedIdentList in Expr                       { Inductive (withSpan $1 $6) $3 $4 [] $6 }
     | let inductive ident TypedIdentList '=' PipeConstrList in Expr    { Inductive (withSpan $1 $8) $3 $4 $6 $8 }
     | 'λ' ident '.' Expr                                               { Lambda (withSpan $1 $4) $2 Nothing $4 }
     | 'λ' '(' ident ':' Expr ')' '.' Expr                              { Lambda (withSpan $1 $8) $3 (Just $5) $8 } 
     | Expr1                                                            { $1 }

Expr1 : Expr2 '->' Expr                         { Pi (withSpan $1 $3) Nothing $1 $3 }
      | '(' ident ':' Expr ')' '->' Expr        { Pi (withSpan $1 $7) (Just $2) $4 $7 }
      | Expr2                                   { $1 }

Expr2 : Expr2 Expr3 { App (withSpan $1 $2) $1 $2 }
      | Expr3      { $1 }

Expr3 : identS          { Var (withSpan (snd $1) (snd $1)) (fst $1) }
      | '?' identS      { NamedHole (withSpan $1 (snd $2)) (fst $2) }
      | '_'             { Hole (withSpan $1 $1) }
      | '(' Expr ')'    { $2 }
      | type            { Type (withSpan $1 $1) }


TypedIdentList : '(' ident ':' Expr ')' TypedIdentList { ($2, $4) : $6 }
               |                                       { [] }

PipeConstrList : '|' ident ':' Expr PipeConstrList { ($2, $4) : $5 }
               |                                   { [] }

{
parseError :: [Token] -> a
parseError [] = error "Parse Error: Unexpected EOF" 
parseError (tok : tokens) = error $ show (tokenSpan tok) <> ": Parse Error: Unexpected tokens '" 
      <> show (map tokenClass (tok : tokens)) <> "'"
}