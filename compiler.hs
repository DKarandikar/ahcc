import System.IO  
import Control.Monad
import Data.Char
import System.Environment

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

---- parser ----

data Tree = ProgNode Tree
          | FuncNode String Tree
          | ExprNode Int
          | StatementNode Tree
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
                                       else error "Must be terminated by }"
                              _ -> error "Invalid func syntax"
                        _ -> error "Invalid func syntax"
                  _ -> error "Invalid func syntax"
            _ -> error $ "Invalid func syntax" ++ show t'
      _ -> error "Invalid func syntax"


statement (t:ts) = 
   case t of
      TokKeyword "return" ->
         let (expTree, tokens) = expr ts
         in (StatementNode expTree, tokens) -- error here
      _ -> error "Invalid func syntax"

expr (t:ts) =
   case t of
      TokNum num -> (ExprNode num, accept ts )
      _ -> error "B"




parse :: [Token] -> Tree
parse toks = let (tree, toks') = program toks
             in
               if null toks' 
               then tree
               else error $ "Leftover tokens: " ++ show toks'

---- evaluator ----
-- show

-- data Tree = ProgNode Tree
--           | FuncNode String Tree
--           | ExprNode Int
--           | StatementNode Tree

evaluate :: Tree -> String
evaluate (FuncNode fname tree) = 
    let x = evaluate tree 
    in ".globl " ++ fname ++ "\n" ++ fname ++ ":" ++ "\n" ++ x

evaluate (ProgNode tree) =  evaluate tree 

evaluate (StatementNode tree) =
    let x = evaluate tree 
    in "    mov   $" ++ x ++ ", %rax\n    ret"

evaluate (ExprNode x) = show x

---- main ----

main :: IO()
main = do  
        let list = []
        args <- getArgs
        handle <- openFile (head args) ReadMode
        contents <- hGetContents handle
      --   print contents
      --   print (lexor contents)
      --   print (parse $ lexor contents)
      --   print (evaluate $ parse $ lexor contents)
        goodbyeFile <- openFile "assembly.s" WriteMode
        hPutStrLn goodbyeFile (evaluate $ parse $ lexor contents)
        hClose goodbyeFile
        hClose handle 