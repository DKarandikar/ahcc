module Lexer(lexor, Token(..)) where

import Data.Char

keywords = ["int", "return"]


data Token = TokLCurlyBrace
           | TokRCulryBrace
           | TokLParen
           | TokRParen
           | TokSemicolon
           | TokKeyword String
           | TokIdentifier String
           | TokNum Int
           | TokEnd
           | TokReservedString String
    deriving (Show, Eq)


lexor :: String -> [Token]
lexor [] = []
lexor (c : cs) 
    | c == '{' = TokLCurlyBrace : lexor cs
    | c == '}' = TokRCulryBrace : lexor cs
    | c == '(' = TokLParen : lexor cs
    | c == ')' = TokRParen : lexor cs
    | c == ';' = TokSemicolon : lexor cs
    | elem c reservedChars = TokReservedString [c] : lexor cs
    | isDigit c = number c cs
    | isAlpha c = identifier c cs
    | isSpace c = lexor cs
    | otherwise = error $ "Cannot lexor " ++ [c]


number :: Char -> String -> [Token]
number c cs = 
   let (digs, cs') = span isDigit cs in
   TokNum (read (c : digs)) : lexor cs'

identifier :: Char -> String -> [Token]
identifier c cs = 
    let (name, cs') = span isAlphaNum cs 
    in if elem (c:name) keywords
        then TokKeyword (c:name) : lexor cs'
        else TokIdentifier (c:name) : lexor cs'
