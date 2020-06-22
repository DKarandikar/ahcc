module Lexer(lexor, Token(..)) where

import Data.Char

keywords = ["int", "return"]

-- Both of these will become TokReservedString
reservedChars = "~!-+/*<>"
reservedStrings = ["&&", "||", "==", "!=", "<=", ">="]


isSpecialChar :: Char -> Bool
isSpecialChar a = elem a (reservedChars ++ (concat reservedStrings))


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
    | isDigit c = number c cs
    | isAlpha c = identifier c cs
    | isSpace c = lexor cs
    | isSpecialChar c = reservedString c cs
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

reservedString :: Char -> String -> [Token]
reservedString c cs = 
    let (name, cs') = span isSpecialChar cs 
    in if length name == 0 
        then 
            if elem c reservedChars
            then TokReservedString [c] : lexor cs
            else TokIdentifier [c] : lexor cs
        else
            if elem (c:name) reservedStrings
            then TokReservedString (c:name) : lexor cs'
            else TokIdentifier (c:name) : lexor cs'
