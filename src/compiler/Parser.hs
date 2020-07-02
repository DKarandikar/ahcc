module Parser(parse, Tree(..)) where

import Lexer (Token(..))

data Tree = ProgNode Tree
          | FuncNode String [Tree]
          | ConstNode Int
          | ReturnNode Tree
          | UnOpNode String Tree
          | BinOpNode String Tree Tree
          | DeclNode String (Maybe Tree)
          | ExprNode Tree
          | VarNode String
          | AssignNode String Tree
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

consumeAndCheckTok :: Token -> [Token] -> [Token]
consumeAndCheckTok tokToCheck (t:ts) = 
   if t == tokToCheck
      then ts
      else error $ "Invalid syntax, expected: " ++ (show tokToCheck)

consumeAndCheckTokIdentifier :: [Token] -> ([Token], String)
consumeAndCheckTokIdentifier (t:ts) = 
   case t of 
      TokIdentifier id -> (ts, id)
      _ -> error $ "Invalid syntax, expected: tokIdentifier"

function toks = 
   let 
      toks' = consumeAndCheckTok (TokKeyword "int") toks
      (toks'', id) = consumeAndCheckTokIdentifier toks'
      toks''' = consumeAndCheckTok TokLParen toks''
      toks'''' = consumeAndCheckTok TokRParen toks'''
      toks''''' = consumeAndCheckTok TokLCurlyBrace toks''''
      (statTrees, tokens) = statements [] toks'''''
      tokens' = consumeAndCheckTok TokRCulryBrace tokens
   in
      (FuncNode id statTrees, accept tokens)

statements :: [Tree] -> [Token] -> ([Tree], [Token])
statements trees (t:ts) = 
   case t of 
      TokRCulryBrace -> (trees, t:ts)
      _ -> let (statTree, tokens) = statement $ t:ts
           in statements (trees ++ [statTree]) tokens


statement (t:ts) = 
   case t of
      TokKeyword "return" ->
         let (expTree, tokens) = expr ts
         in 
            if lookAhead tokens == TokSemicolon
               then (ReturnNode expTree, accept tokens)
               else error ("missing ; tokens left" ++ show ts)
      TokKeyword "int" ->
         let (t':ts') = ts 
         in case t' of 
            TokIdentifier a ->
               let (t'':ts'') = ts' 
               in case t'' of 
                  TokSemicolon -> (DeclNode a Nothing, ts'')
                  TokReservedString s -> if s == "="
                     then 
                        let (expTree, toks) = expr ts''
                        in 
                           if lookAhead toks == TokSemicolon
                              then (DeclNode a (Just expTree), accept toks)
                              else error "Expression should end in ;"
                     else
                        error "Missing ="
                  _ -> error "Need to follow var name with ; or ="
            _ -> error "Missing variable name"   
      _ -> 
         let (exprTree, toks) = expr (t:ts)
         in 
            if lookAhead toks == TokSemicolon
               then (exprTree, accept toks)
               else error "Expression should end in ;" 

expr (t:ts) = 
   case t of 
      TokIdentifier a -> 
         let (t':ts') = ts
         in case t' of
            TokReservedString s -> if s == "=" 
               then 
                  let (exprTree, toks) = expr ts'
                  in (AssignNode a exprTree, toks)
               else 
                  let (tree, tokens) = logicalAndExpr (t:ts)
                  in addThings ["||"] logicalAndExpr tree tokens
            _ ->  
               let (tree, tokens) = logicalAndExpr (t:ts)
               in addThings ["||"] logicalAndExpr tree tokens
      _ ->
         let (tree, tokens) = logicalAndExpr (t:ts)
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
      TokIdentifier a -> (VarNode a, ts)
      _ -> error $ "Invalid factor token: " ++ (show t) ++ " at " ++ (show ts)

parse :: [Token] -> Tree
parse toks = let (tree, toks') = program toks
             in
               if null toks' 
               then tree
               else error $ "Leftover tokens: " ++ show toks'
