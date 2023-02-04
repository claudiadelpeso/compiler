{-# OPTIONS_GHC -Wno-unused-imports #-} -- Turn off unused import warning off in stub
{-# OPTIONS_GHC -Wno-unused-matches #-} -- Turn off unused binding warning off in stub

module Interpreter (interpret, Strategy(..)) where
import Control.Monad.Except

import Data.Map (Map)
import qualified Data.Map as Map

import Fun.Abs
import Fun.Print

---- Evaluation strategy ----
data Strategy
  = CallByName
  | CallByValue

---- Error monad ----
type Err = Except String

---- ENVIRONMENT ----
data Env = Env {
  functions :: Map Ident Val, 
  variables :: Map Ident Val
} deriving (Show)

data Val = VClos Exp Env  
 deriving (Show)

---- OPERATIONS ON ENV ----
lookupVar :: Env -> Ident -> Maybe Val 
lookupVar env id = case Map.lookup id (variables env) of 
  Nothing -> Nothing
  Just v -> Just v 

lookupFun :: Env -> Ident -> Maybe Val 
lookupFun env id = case Map.lookup id (functions env) of
  Nothing -> Nothing
  Just v -> Just v 

lookupEnv :: Env -> Ident -> Val
lookupEnv env id = case lookupVar env id of 
    Just val -> val 
    Nothing -> case lookupFun env id of 
        Just val -> val 
        Nothing -> error $ "(lookupEnv)Var " ++ show id ++ " is not in the environment \n" 
                    ++ "the env is "++show env

extendVar :: Env -> Ident -> Val -> Env
extendVar env id val = Env {functions = functions env, variables = Map.insert id val (variables env)}

extendFun :: Env -> Ident -> Val -> Env
extendFun env id val = Env {functions = Map.insert id val (functions env), variables = variables env}

emptyEnv :: Env 
emptyEnv = Env {
  functions = Map.empty,
  variables = Map.empty
}


---- INTERPRETER PROG ----
interpret :: Strategy -> Program -> Err Integer
interpret s (Prog defs (DMain mainExp)) = do
  let updEnv = evalDef emptyEnv defs -- extend all functions to environment     
  let mainVal = eval updEnv s mainExp -- evaluate main function
  case mainVal of 
    VClos (EInt i) _ -> return i 
    _ -> error "the return value is not correct (it should be an integer)"

evalDef :: Env -> [Def] -> Env
evalDef env [] = env -- 
evalDef env (def@(DDef id args exp):ds) = do 
  let val = fromDefToVal def -- convert arguments to type Val
  let updEnv = extendFun env id val -- extend function with values
  evalDef updEnv ds

--takes the arguments of a function and turns them into Val type
fromDefToVal :: Def -> Val 
fromDefToVal (DDef id [] exp) = VClos exp emptyEnv
fromDefToVal (DDef id (arg:restArgs) exp) = do 
  let (VClos exp' env') = fromDefToVal (DDef id restArgs exp)
  let e = EAbs arg exp' 
  VClos e env'
  
---- INTERPRET EXPRESSIONS ----
eval :: Env -> Strategy -> Exp -> Val 
eval env s (EInt int) = VClos (EInt int) emptyEnv 

eval env s (EVar id) = do 
  let (VClos exp expEnv) = lookupEnv env id 
  let updEnv = Env {functions = functions env , variables = variables expEnv}
  eval updEnv s exp 

eval env s (EAdd e1 e2) = do 
  case (eval env s e1 , eval env s e2) of 
    (VClos (EInt eval1) _, VClos (EInt eval2) _) -> VClos (EInt (eval1 + eval2)) emptyEnv 
    _ -> error "Cannot add two things that are not integers"

eval env s (ESub e1 e2) = do 
  case (eval env s e1, eval env s e2) of 
    (VClos (EInt eval1) _, VClos (EInt eval2) _) -> VClos (EInt (eval1 - eval2)) emptyEnv 
    _ -> error "Cannot substract two things that are not integers"

eval env s (ELt e1 e2) = do
  case (eval env s e1, eval env s e2) of 
    (VClos (EInt eval1) _, VClos (EInt eval2) _) -> if eval1<eval2 then VClos (EInt 1) emptyEnv
                                                    else VClos (EInt 0) emptyEnv
    _ -> error "Cannot compare two things that are not integers"

eval env s (EIf e1 e2 e3) = do 
  case eval env s e1 of 
    (VClos (EInt eval1) updEnv1) -> if eval1==1 then eval env s e2
                                    else eval env s e3
    _ -> error "Cannot compare two things that are not integers"

eval env s e@(EAbs id exp) = do 
  let varEnv = Env {functions = Map.empty, variables= variables env}
  VClos e varEnv 
  
eval env s (EApp exp1 exp2) = do 
  case eval env s exp1 of 
    VClos (EAbs id e) env2 -> do 
      case s of 
        CallByName -> do
                    let updEnv = Env{functions=Map.empty, variables=variables env}
                    let val2 = VClos exp2 updEnv
                    let updEnv' = extendVar Env{functions = functions env, variables = variables env2} id val2
                    eval updEnv' s e

        CallByValue -> do
                    let val2 = eval env s exp2 
                    let updEnv = extendVar Env{functions = functions env, variables = variables env2} id val2
                    eval updEnv s e
    _ -> error "expression does not have correct shape"