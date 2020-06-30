module Evaluator(evaluate) where

import Parser (Tree(..))
import Control.Monad.State 

rax = "%rax"
rcx = "%rcx"

evaluate :: Tree -> String
evaluate tree = evalState (eval tree) 0

type IdCounter = Int
type Evaluator = State IdCounter

eval :: Tree -> Evaluator String

eval (ProgNode tree) = eval tree
eval (ConstNode x) = return $ move ('$': (show x)) rax

eval (ReturnNode tree) = do
    res <- eval tree
    return $ res ++ "    ret"

eval (FuncNode fname tree) = do
    res <- eval tree
    return $ ".globl " ++ fname ++ "\n" ++ fname ++ ":" ++ "\n" ++ res

eval (UnOpNode op tree) = do 
    res <- eval tree 
    return $ case op of 
        "-" -> concat [res, doUnOp "neg" rax]
        "~" -> concat [res, doUnOp "not" rax]
        "!" -> concat [res, doBinOp "cmpq" "$0" rax, move "$0" rax, doUnOp "sete" "%al"]
        _ -> error $ "Invalid unary operation: " ++ op

eval (BinOpNode op tree tree') = do 
    lh_res <- eval tree 
    rh_res <- eval tree'

    case op of 
        "+" -> return $ concat [lh_res, push rax, rh_res, pop rcx, doBinOp "addq" rcx rax]
        "-" -> return $ concat [lh_res, push rax, rh_res, pop rcx, doBinOp "subq" rax rcx, move rcx rax]
        "/" -> return $ concat [lh_res, push rax, rh_res, move rax rcx, pop rax, doOp "cqto", doUnOp "idivq" rcx]
        "*" -> return $ concat [lh_res, push rax, rh_res, pop rcx, doBinOp "imul" rcx rax]
        "==" -> return $ generateComparisonAssem "sete" lh_res rh_res
        "!=" -> return $ generateComparisonAssem "setne" lh_res rh_res
        ">" -> return $ generateComparisonAssem "setg" lh_res rh_res
        "<" -> return $ generateComparisonAssem "setl" lh_res rh_res
        ">=" -> return $ generateComparisonAssem "setge" lh_res rh_res
        "<=" -> return $ generateComparisonAssem "setle" lh_res rh_res
        "||" -> generateOrState lh_res rh_res
        "&&" -> generateAndState lh_res rh_res
        _  -> error $ "Invalid binary operation: " ++ op

generateOrState :: String -> String -> Evaluator String
generateOrState lh_res rh_res = do
    counter <- get
    let 
        x = show counter
        x' = show $ counter + 1 
        s = concat
            [ lh_res
            , doBinOp "cmpq" "$0" rax
            , "    je  " ++ "_id" ++ x ++ "\n"
            , move "$1" rax
            , "    jmp " ++ "_id" ++ x' ++ "\n"
            , "_id" ++ x ++ ":\n"
            , rh_res
            , doBinOp "cmpq" "$0" rax
            , move "$0" rax
            , doUnOp "setne" "%al"
            ,  "_id" ++ x' ++ ":\n" ]
    put (counter + 2)
    return s

generateAndState :: String -> String -> Evaluator String
generateAndState lh_res rh_res = do
    counter <- get
    let 
        x = show counter
        x' = show $ counter + 1 
        s = concat
            [ lh_res
            , doBinOp "cmpq" "$0" rax
            , "    jne  " ++ "_id" ++ x ++ "\n"
            , "    jmp " ++ "_id" ++ x' ++ "\n"
            , "_id" ++ x ++ ":\n"
            , rh_res
            , doBinOp "cmpq" "$0" rax
            , move "$0" rax
            , doUnOp "setne" "%al"
            ,  "_id" ++ x' ++ ":\n" ]
    put (counter + 2)
    return s

generateComparisonAssem:: String -> String -> String -> String
generateComparisonAssem op lh_res rh_res = concat [lh_res, push rax, rh_res, pop rcx, doBinOp "cmpq" rax rcx, move "$0" rax, doUnOp op "%al"]

move :: String -> String -> String
move v1 v2 = concat ["    movq " ++ v1, ", ", v2, "\n"]

push :: String -> String
push v = concat ["    push ", v, "\n"]

pop :: String -> String
pop v = concat ["    pop ", v, "\n"]

doBinOp:: String -> String -> String -> String
doBinOp op v1 v2 = concat ["    ", op, " ", v1, ", ", v2, "\n"]

doUnOp:: String -> String -> String
doUnOp op v = concat ["    ", op, " ", v, "\n"]

doOp:: String -> String
doOp op = concat ["    ", op, "\n"]
