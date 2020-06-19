module Parser(parse, Tree(..)) where

import Lexer (Token(..))

data Tree = ProgNode Tree
          | FuncNode String Tree
          | ConstNode Int
          | ReturnNode Tree
          | UnOpNode Char Tree
          | BinOpNode Char Tree Tree
    deriving Show

lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (t:ts) = t

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts

program :: [Token] -> (Tree, [Token])
function :: [Token] -> (Tree, [Token])
statement :: [Token] -> (Tree, [Token])
expr :: [Token] -> (Tree, [Token])

program ts = 
   let (tree, ts') = function ts
   in (ProgNode tree, ts')

function (t:ts) = 
   case t of
      TokKeyword "int" ->
         let (t':ts') = ts
         in case t' of
            TokIdentifier id -> 
               let (t'':ts'') =  ts'
               in case t'' of 
                  TokLParen -> 
                     let (t''':ts''') = ts''
                     in case t''' of
                        TokRParen -> 
                           let (t'''':ts'''') = ts'''
                           in case t'''' of
                              TokLCurlyBrace -> 
                                 let (statTree, tokens) = statement ts''''
                                 in 
                                    if lookAhead tokens == TokRCulryBrace
                                       then (FuncNode id statTree, accept tokens)
                                       else error "Invalid func syntax: must be terminated by }"
                              _ -> error "Invalid func syntax: missing {"
                        _ -> error "Invalid func syntax: no params yet, need )"
                  _ -> error "Invalid func syntax: missing ( for params"
            _ -> error $ "Invalid func syntax: missing func identifier"
      _ -> error "Invalid func syntax: no return type"


statement (t:ts) = 
   case t of
      TokKeyword "return" ->
         let (expTree, tokens) = expr ts
         in 
            if lookAhead tokens == TokSemicolon
               then (ReturnNode expTree, accept tokens)
               else error $ "missing ;"
      _ -> error "return expected"

expr toks =
   let (termTree, tokens) = term toks
   in addTerms termTree tokens

addTerms :: Tree -> [Token] -> (Tree, [Token])
addTerms tree ts = 
   case lookAhead ts of 
      TokReservedChar op ->
         if elem op "+-"
            then let (termTree, tokens) = term $ accept ts
                 in addTerms (BinOpNode op tree termTree) tokens
            else (tree, ts)
      _ ->  (tree, ts)

term toks = 
   let (factorTree, tokens) = factor toks
   in addFactors factorTree tokens

addFactors :: Tree -> [Token] -> (Tree, [Token])
addFactors tree ts = 
   case lookAhead ts of 
      TokReservedChar op ->
         if elem op "/*"
            then let (factorTree, tokens) = factor $ accept ts
                 in addFactors (BinOpNode op tree factorTree) tokens
            else (tree, ts)
      _ ->  (tree, ts)

factor (t:ts) = 
   case t of 
      TokNum num -> (ConstNode num, ts)
      TokReservedChar op ->
         let (factorTree, ts') = factor ts
         in (UnOpNode op factorTree, ts') 
      TokLParen -> 
         let (exprTree, ts') = expr ts
         in 
            if lookAhead ts' == TokRParen
               then (exprTree, accept ts')
               else error "Factor must end in )" 


parse :: [Token] -> Tree
parse toks = let (tree, toks') = program toks
             in
               if null toks' 
               then tree
               else error $ "Leftover tokens: " ++ show toks'
