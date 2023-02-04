module Interpreter where

import Control.Monad
import Control.Exception

import System.Exit

import Data.Map (Map,insert,update)
import qualified Data.Map as Map
import Data.Maybe

import CMM.Abs
import CMM.Print
import CMM.ErrM

import TypeChecker (AnnotatedProgram)
import GHC.Real (underflowError)

type InterpreterError = String
type Env = (Sign, [Context]) 
type Sign = Map Id Def 
type Context = Map Id Val  
data Val = VInt Integer | VDouble Double | VBool Bool | VVoid | Ret Val
    deriving (Show)

instance Ord Val where
    (VInt i) <= (VDouble d) = fromIntegral i <= d
    (VDouble i) <= (VInt d) = i <=  fromIntegral d
    (VDouble i) <= (VDouble d) = i <= d
    (VInt i) <= (VInt d) = i <= d
    _ <= _ = error "stop comparing shit"

instance Eq Val where
    (VInt i) == (VDouble d) = fromIntegral i == d
    (VDouble i) == (VInt d) = i ==  fromIntegral d
    (VDouble i) == (VDouble d) = i == d
    (VInt i) == (VInt d) = i == d
    VVoid == VVoid = True
    (VBool b1) == (VBool b2) = b1 == b2
    a == b = error $ "stop comparing shit as in a: " ++ show a ++"and b: " ++ show b

-------------- tests ----------
testdef1 = DFun Type_int (Id "print") ([]) ([])
testdef2 = DFun Type_int (Id "doubleInt") [ADecl Type_int (Id "toDouble")] []
testsign = insert (Id "main") testdef1 Map.empty
testEnv = (testsign, [insert (Id "num") (VInt 3) Map.empty, insert (Id "dbl") (VDouble 3.4) Map.empty])

-------------- env ------------
-- checks if a variable is in the context ✅
lookupVar :: Env -> Id -> IO Val
lookupVar (_,[]) id = return VVoid 
lookupVar (sig, c:cs) id = case Map.lookup id c of
    Just val -> return val
    Nothing -> lookupVar (sig,cs) id

lookupVarFails :: Env -> Id -> IO Val
lookupVarFails (_,[]) id = fail $ show id ++ " not in environment"
lookupVarFails (sig, c:cs) id = case Map.lookup id c of
    Just VVoid -> fail $ show id ++ " has no value"
    Just val -> return val
    Nothing -> lookupVarFails (sig,cs) id

-- checks if a function is in sign ✅
lookupFun :: Env -> Id -> IO Def
lookupFun (sig, _) id = case Map.lookup id sig of
    Just d -> return d
    Nothing ->  return $ DFun Type_void (Id "") [] []

-- checks if a variable is in environment, if not then it adds it ✅
extendVar :: Env -> Id -> Val -> IO Env
extendVar (sig,[]) id v =  fail $ show id ++ " is not in environment so it cannot be added to environment" 
extendVar (sig,c:cs) id v = do 
    val <- lookupVar (sig,c:cs) id
    return (sig, Map.insert id v c:cs)

-- checks if function is in environment, if not then adds it ✅
extendFun :: Env -> Def -> IO Env 
extendFun env@(sig, c) f@(DFun varType id args _) = do
    func <- lookupFun env id
    case func of
        DFun Type_void (Id "") [] [] -> return (insert id f sig, c)
        _ -> fail $ show id ++ " is already in environment so it cannot be added" 
        
--update the value of an already existing variable in context ✅ 
updateVar :: Env -> Id -> Val -> IO Env -- DOUBLE CHECK
updateVar e@(sig, []) id _  = fail $ "Variable " ++ show id ++ " not in scope"
updateVar (sig, c:cs) id v = do 
    let val = Map.lookup id c
    case val of 
        Just a -> do
            let f _ = v
            let newEnv = Map.adjust f id c
            return (sig, newEnv:cs)
        Nothing -> do
            (_,tail) <- updateVar (sig, cs) id v
            return (sig,c:tail)

-- create a new block ✅
newBlock :: Env -> Env 
newBlock (sig, c) = (sig, Map.empty:c)

-- exit the block ✅
exitBlock :: Env -> Env
exitBlock (sig, c:cs) = (sig, cs)

-- empty the environment ✅
emptyEnv :: Env
emptyEnv = (Map.empty, [Map.empty])

--------- interpret expressions --------

-- takes an expression and gives its value based on variables in environment
evalExp :: Env -> Exp -> IO(Val,Env)
evalExp env ex = case ex of
    (ETyped (EBool b) givenTp) -> if b == LTrue 
        then return (VBool True, env) 
        else return (VBool False, env)
    (ETyped (EInt n) givenTp)  -> return (castValue (VInt n) givenTp,env)
    (EDouble d) -> return (VDouble d, env)
    (EId id)  -> do
        value <- lookupVarFails env id
        return (value, env)
    (EApp i exps) -> do
        (updEnv,val) <- applyFun env i exps
        return (val,updEnv)
    (EPre op id) -> do
        value <- lookupVarFails env id 
        let resultOp = pVal op value
        newEnv <- updateVar env id resultOp
        case resultOp of
            VInt i -> return (VInt i,newEnv)
            VDouble d -> return(VDouble d, newEnv)  
    (EPost id op) -> do
        value <- lookupVarFails env id 
        let resultOp = pVal op value
        newEnv <- updateVar env id resultOp
        return (value,newEnv)
    (ETyped (EMul e1 op e2) givenTp) -> do
        (u, upenv1) <- evalExp env (castExp e1 givenTp)
        (v,upenv2) <- evalExp upenv1 (castExp e2 givenTp)
        return (mulValue op u v , upenv2)
    (EAdd e1 op e2) -> do
        (u,upenv1) <- evalExp env e1
        (v,upenv2) <- evalExp upenv1 e2
        return (addValue op u v , upenv2)
    (ECmp e1 op e2) -> do
        (u,upenv1) <- evalExp env e1
        (v,upenv2) <- evalExp upenv1 e2
        return (compareVal op u v, upenv2)
    (EAnd e1 e2) -> do 
        (VBool u,updEnv) <- evalExp env e1
        if u then do
            (VBool v,updEnv') <- evalExp env e2
            return (VBool (u&&v), updEnv')
        else return (VBool False, updEnv)
    (EOr e1 e2) -> do 
        (VBool u,updEnv) <- evalExp env e1
        if not u then do
            (VBool v,updEnv') <- evalExp env e2
            return (VBool (u||v), updEnv')
        else return (VBool True, updEnv)
    (EAss id e) -> do
        (evaluation, env) <- evalExp env e
        env2 <- updateVar env id evaluation
        return (evaluation, env2)
    (ETyped e tp) -> evalExp env e

-- when there is a print statment, print only the value and not the type of the value
printSole :: Val -> IO ()
printSole (VInt b)    = print b
printSole (VDouble i) = print i
printSole (VBool d)   = print d
printSole VVoid       = print VVoid
printSole (Ret v)     = printSole v

-- helper function for EApp 
applyFun :: Env -> Id -> [Exp] -> IO (Env,Val)
applyFun env (Id "printInt") [exp] = do
    (v,updEnv) <- evalExp env exp
    printSole v
    return (updEnv,VVoid)
applyFun env (Id "printDouble") [exp] = do
    (v,updEnv) <- evalExp env (castExp exp Type_double)
    printSole v 
    return (updEnv,VVoid)
applyFun env (Id "printInt") e = error "no"
applyFun env (Id "printDouble") e = error "no"

applyFun env (Id "readInt") _ = do
    num <- getLine
    return (env,VInt $ read num)
applyFun env (Id "readDouble") _ = do
    num <- getLine
    return (env,VDouble $ read num)

applyFun env generalId args = do
    (evalArgs, updEnv) <- evaluateEachExp env args          -- Evaluate each of the expressions
    (DFun tp _ params stms) <- lookupFun env generalId  
    envForEvalStms <- multExtendVar updEnv params evalArgs  -- Enter the arguments into the enviroment
    let newEnv' = newBlock envForEvalStms                   -- Open a new block for the function scope
    envWReturnStm <- evalStms newEnv' stms                  -- Eval the stms, the return has sole purpose return stm
    val <- lookupVar envWReturnStm (Id "return")            -- Find the return value
    return (updEnv, val)                                  -- Return env with arguments inserted, value of return stm
    
-- evaluate each expression in list of expressions and return each value and updated env
evaluateEachExp :: Env -> [Exp] -> IO ([Val], Env)
evaluateEachExp env [] = return ([],env)
evaluateEachExp env (e:es) = do
    (v,updEnv) <- evalExp env e
    (vs, finalEnv) <- evaluateEachExp updEnv es
    return (v:vs, finalEnv)

multExtendVar :: Env -> [Arg] -> [Val] -> IO Env
multExtendVar env [] []                   = return env
multExtendVar env ((ADecl paramTp i):is) (v:vs) = do
    let castedVal = castValue v paramTp
    updEnv <- extendVar env i castedVal
    multExtendVar updEnv is vs
multExtendVar env args vals = error $ "\nargs: " ++ show args ++ "\nvals: " ++ show vals

castValue :: Val -> Type -> Val
castValue (VInt i) Type_double = VDouble (fromIntegral i)
castValue val _                = val

castExp :: Exp -> Type -> Exp
castExp (ETyped e _) Type_double = ETyped e Type_double
castExp e _                        = e
    
-- used in EPost and EPre operations to add or substract
pVal :: IncDecOp -> Val -> Val
pVal op v = case op of
     OInc -> addValue OPlus v (VInt 1) 
     ODec -> addValue OMinus v (VInt 1)

-- used in EAnd and EOr to compute the value of the logical expresion
logicalVal :: Exp -> Bool -> Bool -> Val
logicalVal (EAnd e1 e2) v1 v2 |v1 && v2 = VBool True
                     | otherwise = error "err in and"
logicalVal (EOr e1 e2) v1 v2 | not v1 && not v2 = VBool False
                     | otherwise = VBool True

-- used in EAdd to sum or substract two expresions
addValue :: AddOp -> Val -> Val -> Val
addValue op (VInt v1) (VInt v2) = case op of
    OPlus -> VInt $ v1 + v2
    OMinus -> VInt $ v1 - v2
addValue op (VInt v1) (VDouble v2) = case op of
    OPlus -> VDouble $ fromIntegral v1 + v2
    OMinus -> VDouble $ fromIntegral v1 - v2
addValue op (VDouble v1) (VInt v2) = case op of
    OPlus -> VDouble $ v1 + fromIntegral v2
    OMinus -> VDouble $ v1 - fromIntegral v2
addValue op (VDouble v1) (VDouble v2) = case op of
    OPlus -> VDouble $ v1 + v2
    OMinus -> VDouble $ v1 - v2
addValue op v1 v2 = error (show v1 ++ " and " ++ show v2 ++ " can not arithmetically paired")

-- used in EMul to multiply or divide two expressions
mulValue :: MulOp -> Val -> Val -> Val
mulValue op (VInt v1) (VInt v2) = case op of
    OTimes -> VInt $ v1 * v2
    ODiv -> VInt $ v1 `div` v2
mulValue op (VInt v1) (VDouble v2) = case op of
    OTimes -> VDouble $ fromIntegral v1 * v2
    ODiv -> VDouble $ fromIntegral v1 / v2
mulValue op (VDouble v1) (VInt v2) = case op of
    OTimes -> VDouble $ v1 * fromIntegral v2
    ODiv -> VDouble $ v1 / fromIntegral v2
mulValue op (VDouble v1) (VDouble v2) = case op of
    OTimes -> VDouble $ v1 * v2
    ODiv -> VDouble $ v1 / v2

-- used in ECmp to compare two expressions
compareVal :: CmpOp -> Val -> Val -> Val
compareVal op v1 v2 = case op of
    OLt -> if v1 < v2 then VBool True else VBool False
    OGt -> if v1 > v2 then VBool True else VBool False
    OLtEq -> if v1 <= v2 then VBool True else VBool False
    OGtEq -> if v1 >= v2 then VBool True else VBool False
    OEq -> if v1 == v2 then VBool True else VBool False
    ONEq -> if v1 /= v2 then VBool True else VBool False

------------- interpret statments --------
hasReturned :: Env -> IO Bool
hasReturned env = do
    returned <- lookupVar env (Id "return") 
    case returned of
        VVoid -> return False
        _ -> return True

upReturn :: Env -> IO Env
upReturn env@(f, c:cs) = do
    returned <- lookupVar env (Id "return")
    extendVar (exitBlock env) (Id "return") returned

-- takes an expression and returns its evaluated value
evalStm :: Env -> Stm -> IO Env
evalStm env stm = do
    returned <- hasReturned env
    if returned 
        then return env
    else case stm of 
        (SExp e) -> do
            (_,updEnv) <- evalExp env e
            return updEnv
        (SDecls tp  ids) -> declareId env ids
        (SInit tp id (ETyped e  _)) -> do
            (v,updatedEnv) <- evalExp env (ETyped e tp) 
            extendVar updatedEnv id v
        (SReturn e) -> do
            (v,updatedEnv) <- evalExp env e 
            extendVar updatedEnv (Id "return") v 
        (SWhile e stm) -> do 
            (VBool b, updatedEnv)<-evalExp env e
            if not b 
                then return updatedEnv
            else do
                (sig, cs) <- evalStm (newBlock updatedEnv) stm
                newEnv' <- upReturn (sig, cs)
                
                evalStm newEnv' (SWhile e stm)
        (SBlock stms) -> do 
            updatedEnv <- evalStms (newBlock env) stms
            upReturn updatedEnv
        (SIfElse e s1 s2) -> do
            (VBool b, updatedEnv) <- evalExp env e
            if b 
                then do 
                    newEnv <- evalStms (newBlock updatedEnv) [s1]
                    upReturn newEnv               
                else do 
                    newEnv <- evalStms (newBlock updatedEnv) [s2]
                    upReturn newEnv

-- it takes several ids and adds them to the environment
declareId :: Env -> [Id] -> IO Env
declareId = foldM (\ env id -> extendVar env id VVoid)

-- it takes several stms and evaluates them one by one
evalStms :: Env -> [Stm] -> IO Env
evalStms env [] = return env
evalStms env (s:ss) = do 
    possibleReturn <- lookupVar env (Id "return")
    case possibleReturn of
        VVoid -> do
                updatedEnv <- evalStm env s 
                evalStms updatedEnv ss
        _ -> return env
    
------------- interpret definitions ---------

-- evaluates a definition 
evalDef :: Env -> Def -> IO Env
evalDef env (DFun tp id args stms) = do
    let updEnv = newBlock env
    updEnv2 <- evalArg updEnv args
    evalStms updEnv2 stms

-- evalutates the arguments
evalArg :: Env -> [Arg] -> IO Env
evalArg env [] = return env
evalArg env ((ADecl tp id):as) = do
    updEnv <- declareId env [id]
    evalArg updEnv as

------------- interpret program --------

interpret :: AnnotatedProgram -> IO ()
interpret (PDefs []) = return ()
interpret (PDefs defs) = do
    updEnv <- foldM extendFun emptyEnv defs
    mainFun <- lookupFun updEnv (Id "main")
    updEnv' <- evalDef updEnv mainFun
    result <- lookupVar updEnv' (Id "return")
    return ()