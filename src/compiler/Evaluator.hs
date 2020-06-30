module Evaluator(evaluate) where

import Parser (Tree(..))

rax = "%rax"
rcx = "%rcx"

evaluate :: Tree -> String
evaluate tree = getString $ eval 0 tree

type AsmState = (Int, String)

getString :: AsmState -> String
getString (_, a) = a

getEvalString :: Int -> Tree -> String
getEvalString counter tree = getString (eval counter tree)

eval :: Int -> Tree -> AsmState
eval counter (ProgNode tree) =  eval counter tree 
eval counter (FuncNode fname tree) = (counter, ".globl " ++ fname ++ "\n" ++ fname ++ ":" ++ "\n" ++ (getEvalString counter tree))
eval counter (ReturnNode tree) = (counter, (getEvalString counter tree) ++ "    ret")

eval counter (ConstNode x) =  (counter, move ('$': (show x)) rax)
eval counter (UnOpNode op tree)
    | op == "-" = (counter, concat [getEvalString counter tree, doUnOp "neg" rax])
    | op == "~" = (counter, concat [getEvalString counter tree, doUnOp "not" rax])
    | op == "!" = (counter, concat [getEvalString counter tree, doBinOp "cmpq" "$0" rax, move "$0" rax, doUnOp "sete" "%al"])
    | otherwise = error $ "Invalid unary operation: " ++ op

eval counter (BinOpNode op tree tree') = (counter + case op of 
    "||" -> 2
    _ -> 0
    , case op of 
    "+" -> concat [getEvalString counter tree, push rax, getEvalString counter tree', pop rcx, doBinOp "addq" rcx rax]
    "-" -> concat [getEvalString counter tree, push rax, getEvalString counter tree', pop rcx, doBinOp "subq" rax rcx, move rcx rax]
    "/" -> concat [getEvalString counter tree, push rax, getEvalString counter tree', move rax rcx, pop rax, doOp "cqto", doUnOp "idivq" rcx]
    "*" -> concat [getEvalString counter tree, push rax, getEvalString counter tree', pop rcx, doBinOp "imul" rcx rax]
    "==" -> generateComparisonAssem counter "sete" tree tree'
    "!=" -> generateComparisonAssem counter "setne" tree tree'
    ">" -> generateComparisonAssem counter "setg" tree tree'
    "<" -> generateComparisonAssem counter "setl" tree tree'
    ">=" -> generateComparisonAssem counter "setge" tree tree'
    "<=" -> generateComparisonAssem counter "setle" tree tree'
    "||" -> generateOrAsmState counter tree tree'
    _  -> error $ "Invalid binary operation: " ++ op)

generateComparisonAssem:: Int -> String -> Tree -> Tree -> String
generateComparisonAssem counter op tree tree' = concat[getEvalString counter tree, push rax, getEvalString counter tree', pop rcx, doBinOp "cmpq" rax rcx, move "$0" rax, doUnOp op "%al"]

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

generateOrAsmState:: Int -> Tree -> Tree -> String
generateOrAsmState counter tree tree' =  
    let 
        x = show counter
        x' = show $ counter + 1 
        s = concat
            [ getEvalString counter tree
            , doBinOp "cmpq" "$0" rax
            , "    je  " ++ "_id" ++ x ++ "\n"
            , move "$1" rax
            , "    jmp " ++ "_id" ++ x' ++ "\n"
            , "_id" ++ x ++ ":\n"
            , getEvalString counter tree'
            , doBinOp "cmpq" "$0" rax
            , move "$0" rax
            , doUnOp "setne" "%al"
            ,  "_id" ++ x' ++ ":\n" ]
    in 
        s
