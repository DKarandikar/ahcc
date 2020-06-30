module Parser(parse, Tree(..)) where

import Lexer (Token(..))

data Tree = ProgNode Tree
          | FuncNode String Tree
          | ConstNode Int
          | ReturnNode Tree
          | UnOpNode String Tree
          | BinOpNode String Tree Tree
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
additiveExpr :: [Token] -> (Tree, [Token])

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
               else error ("missing ; tokens left" ++ show ts)
      _ -> error "return expected"

expr toks = 
   let (tree, tokens) = logicalAndExpr toks
   in addThings ["||"] logicalAndExpr tree tokens

logicalAndExpr toks = 
   let (tree, tokens) = bitwiseOrExpr toks
   in addThings ["&&"] bitwiseOrExpr tree tokens

bitwiseOrExpr toks = 
   let (tree, tokens) = bitwiseXorExpr toks
   in addThings ["|"] bitwiseXorExpr tree tokens

bitwiseXorExpr toks = 
   let (tree, tokens) = bitwiseAndExpr toks
   in addThings ["^"] bitwiseAndExpr tree tokens

bitwiseAndExpr toks = 
   let (tree, tokens) = equalityExpr toks
   in addThings ["&"] equalityExpr tree tokens

equalityExpr toks = 
   let (tree, tokens) = relationalExpr toks
   in addThings ["==", "!="] relationalExpr tree tokens

relationalExpr toks = 
   let (tree, tokens) = bitwiseShiftExpr toks
   in addThings ["<", ">", "<=", ">="] bitwiseShiftExpr tree tokens

bitwiseShiftExpr toks =
   let (tree, tokens) = additiveExpr toks
   in addThings ["<<", ">>"] additiveExpr tree tokens

additiveExpr toks =
   let (termTree, tokens) = term toks
   in addThings ["+", "-"] term termTree tokens

term toks = 
   let (factorTree, tokens) = factor toks
   in addThings ["/", "*", "%"] factor factorTree tokens

-- Recursively adds more expressions of type subExpression, e.g. in <additive-exp> ::= <term> { ("+" | "-") <term> }
-- this will recursively add terms until no more stringsToCheck (+ or -) are left
addThings :: [String] -> ([Token] -> (Tree, [Token])) -> Tree -> [Token] -> (Tree, [Token])
addThings stringsToCheck subExpression tree ts = 
   case lookAhead ts of 
      TokReservedString op ->
         if elem op stringsToCheck
            then let (factorTree, tokens) = subExpression $ accept ts
                 in addThings stringsToCheck subExpression (BinOpNode op tree factorTree) tokens
            else (tree, ts)
      _ ->  (tree, ts)

factor (t:ts) = 
   case t of 
      TokNum num -> (ConstNode num, ts)
      TokReservedString op ->
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
