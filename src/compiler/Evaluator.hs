module Evaluator(evaluate) where

import Parser (Tree(..))
import Control.Monad.State
import qualified Data.Map as M

rax = "%rax"
rcx = "%rcx"

evaluate :: Tree -> String
evaluate tree = evalState (eval tree) (0, -8, M.empty)

type IdCounter = Int
type StackIndex = Int
type VariableMap = M.Map String Int
type EvaluatorState = (IdCounter, StackIndex, VariableMap)
type Evaluator = State EvaluatorState

eval :: Tree -> Evaluator String

eval (ProgNode tree) = eval tree
eval (ConstNode x) = return $ move ('$': (show x)) rax

eval (ReturnNode tree) = do
    res <- eval tree
    return $ concat [res, move "%rbp" "%rsp", doUnOp "pop" "%rbp", "    ret"]  -- fn epilogue

eval (FuncNode fname trees) = do
    ress <- mapM eval trees
    return $ concat [".globl ", fname, "\n", fname, ":\n", doUnOp "push" "%rbp", move "%rsp" "%rbp", concat ress]  --fn prologue

eval (UnOpNode op tree) = do 
    res <- eval tree 
    return $ case op of 
        "-" -> concat [res, doUnOp "neg" rax]
        "~" -> concat [res, doUnOp "not" rax]
        "!" -> concat [res, doBinOp "cmpq" "$0" rax, move "$0" rax, doUnOp "sete" "%al"]
        _ -> error $ "Invalid unary operation: " ++ op

eval (DeclNode varName maybeTree) = do
    ensureVariableNotPresent varName
    case maybeTree of 
        Just v -> do
            addSymbol varName
            initialVal <- eval v
            return $ concat [initialVal, push rax]
        Nothing -> do
            addSymbol varName
            return $ concat [move "$0" rax, push rax]

eval (AssignNode varName tree) = do
    res <- eval tree
    varLocation <- lookUp varName
    return $ concat [res, move rax ((show varLocation) ++ "(%rbp)")]

eval (VarNode varName) = do
    varLocation <- lookUp varName
    return $ move ((show varLocation) ++ "(%rbp)") rax

eval (BinOpNode op tree tree') = do 
    lh_res <- eval tree 
    rh_res <- eval tree'

    case op of 
        "+" -> return $ concat [lh_res, push rax, rh_res, pop rcx, doBinOp "addq" rcx rax]
        "|" -> return $ concat [lh_res, push rax, rh_res, pop rcx, doBinOp "xorq" rcx rax]
        "^" -> return $ concat [lh_res, push rax, rh_res, pop rcx, doBinOp "orq" rcx rax]
        "&" -> return $ concat [lh_res, push rax, rh_res, pop rcx, doBinOp "andq" rcx rax]
        "<<" -> return $ concat [lh_res, push rax, rh_res, move rax rcx, pop rax, doBinOp "shl" "%cl" rax]
        ">>" -> return $ concat [lh_res, push rax, rh_res, move rax rcx, pop rax, doBinOp "shr" "%cl" rax]
        "%" -> return $ concat [lh_res, push rax, rh_res, move rax rcx, pop rax, doOp "cqto", doUnOp "idivq" rcx, move "%rdx" rax]
        "-" -> return $ concat [lh_res, push rax, rh_res, pop rcx, doBinOp "subq" rax rcx, move rcx rax]
        "/" -> return $ concat [lh_res, push rax, rh_res, move rax rcx, pop rax, doOp "cqto", doUnOp "idivq" rcx]
        "*" -> return $ concat [lh_res, push rax, rh_res, pop rcx, doBinOp "imul" rcx rax]
        "==" -> return $ generateComparisonAsm "sete" lh_res rh_res
        "!=" -> return $ generateComparisonAsm "setne" lh_res rh_res
        ">" -> return $ generateComparisonAsm "setg" lh_res rh_res
        "<" -> return $ generateComparisonAsm "setl" lh_res rh_res
        ">=" -> return $ generateComparisonAsm "setge" lh_res rh_res
        "<=" -> return $ generateComparisonAsm "setle" lh_res rh_res
        "||" -> generateOrAndState generateOrAsm lh_res rh_res
        "&&" -> generateOrAndState generateAndAsm lh_res rh_res
        _  -> error $ "Invalid binary operation: " ++ op

ensureVariableNotPresent :: String -> Evaluator ()
ensureVariableNotPresent str = do
    (_, _, map) <- get
    case M.lookup str map of
      Just v  -> error $ "Variable already defined: " ++ str
      Nothing -> return ()

lookUp :: String -> Evaluator Int
lookUp str = do
    (_, _, map) <- get
    case M.lookup str map of
      Just v  -> return v
      Nothing -> error $ "Undefined variable " ++ str

addSymbol :: String -> Evaluator ()
addSymbol varName = do 
    (c, stack, map) <- get
    put $ (c, stack - 8, M.insert varName stack map)
    return ()

generateOrAndState :: (String -> String -> String -> String -> String) -> String -> String -> Evaluator String
generateOrAndState asmGenerator lh_res rh_res = do
    (counter, s, m) <- get
    put (counter + 2, s, m)
    let 
        id_1 = "_id" ++ (show counter)
        id_2 = "_id" ++ (show (counter + 1))
    return $ asmGenerator lh_res rh_res id_1 id_2


generateOrAsm lh_res rh_res id_1 id_2 = concat[
    lh_res, doBinOp "cmpq" "$0" rax, doLabelOp "je" id_1, move "$1" rax, doLabelOp "jmp" id_2, 
    labelLine id_1, rh_res, doBinOp "cmpq" "$0" rax, move "$0" rax, doUnOp "setne" "%al", 
    labelLine id_2 ]

generateAndAsm lh_res rh_res id_1 id_2 = concat[
    lh_res, doBinOp "cmpq" "$0" rax, doLabelOp "jne" id_1, doLabelOp "jmp" id_2, 
    labelLine id_1, rh_res, doBinOp "cmpq" "$0" rax, move "$0" rax, doUnOp "setne" "%al", 
    labelLine id_2 ]

generateComparisonAsm op lh_res rh_res = concat [lh_res, push rax, rh_res, pop rcx, doBinOp "cmpq" rax rcx, move "$0" rax, doUnOp op "%al"]

doLabelOp op id = concat ["    ", op, " ", id, "\n"]

labelLine id = concat [id, ":", "\n"]

move v1 v2 = concat ["    movq " ++ v1, ", ", v2, "\n"]

push v = concat ["    push ", v, "\n"]

pop v = concat ["    pop ", v, "\n"]

doBinOp op v1 v2 = concat ["    ", op, " ", v1, ", ", v2, "\n"]

doUnOp op v = concat ["    ", op, " ", v, "\n"]

doOp op = concat ["    ", op, "\n"]
