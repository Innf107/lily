{
{-# LANGUAGE NoStrictData #-}
module Lily.Parser (parse) where

import Prelude

import Lily.Syntax (Expr(..), Pass(..), Name)
import Lily.Lexer (Token(..))
}

%name parse Expr
%tokentype { Token }
%error { parseError }

%token ident            { IDENT $$ }
%token 'λ'              { LAMBDA }
%token let              { LET }
%token in               { IN }
%token '?'              { QUESTIONMARK }
%token '='              { EQUALS }
%token '.'              { DOT }
%token '('              { LPAREN }
%token ')'              { RPAREN }
%token '->'             { ARROW }
%token '_'              { UNDERSCORE }
%token ':'              { COLON }
%token type             { TYPE }

%%

Expr :: { Expr Parsed }
Expr : let ident '=' Expr in Expr                       { Let $2 Nothing $4 $6 }
     | let ident ':' Expr '=' Expr in Expr              { Let $2 (Just $4) $6 $8}
     | 'λ' ident '.' Expr                               { Lambda $2 Nothing $4 }
     | 'λ' '(' ident ':' Expr ')' '.' Expr              { Lambda $3 (Just $5) $8 } 
     | Expr1                                            { $1 }

Expr1 : Expr2 '->' Expr                         { Arrow $1 $3 }
      | '(' ident ':' Expr ')' '->' Expr        { Pi $2 $4 $7 }
      | Expr2                                   { $1 }

Expr2 : Expr2 Expr { App $1 $2 }
      | Expr3      { $1 }

Expr3 : ident           { Var $1 }
      | '?' ident       { NamedHole $2 }
      | '_'             { Hole }
      | '(' Expr ')'    { $2 }
      | type            { Type }



{
parseError :: [Token] -> a
parseError tokens = error ("Parse Error: Unexpected tokens '" <> show tokens <> "'")
}