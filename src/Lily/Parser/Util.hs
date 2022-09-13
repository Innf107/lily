module Lily.Parser.Util (
    withSpan,
    pattern IdentToken,
) where

import Lily.Lexer
import Lily.Prelude

-- This is technically a bit unsafe, so we really don't want to put it in
-- @Lily.Span@.
withSpan :: (Spanned a, Spanned b) => a -> b -> Span
withSpan a b =
    let s1 = spanOf a
     in let s2 = spanOf b
         in UnsafeMkSpan
                { sourceFile = sourceFile s1
                , startLine = startLine s1
                , startCol = startCol s1
                , endLine = endLine s2
                , endCol = endCol s2
                }

-- Workaround to return multiple values in a Happy token pattern
asIdentToken :: Token -> Maybe (Text, Span)
asIdentToken (Token (IDENT ident) tokenSpan) = Just (ident, tokenSpan)
asIdentToken _ = Nothing

pattern IdentToken :: (Text, Span) -> Token
pattern IdentToken t <- (asIdentToken -> Just t)
    
