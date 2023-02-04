module TypeChecker where

import Control.Monad

import Data.Map (Map, insert, lookup)
import qualified Data.Map as Map

import CMM.Abs
import CMM.Print(printTree)

type AnnotatedProgram = Program
type TypeError = String

---------- env ----------
type Env = (Sign, [Context])
type FuncType = ([Type], Type) 
type Sign = Map Id FuncType 
type Context = Map Id Type  

extendFunc :: Env -> Def -> Either TypeError Env 
extendFunc env@(sig, c) (DFun varType id args _) = do
    let func = lookupFunc env id
    case func of
        Left _ -> 
            Right (insert id (argsType, varType) sig, c)
            where argsType               = map argType args
                  argType (ADecl tp aId) = tp
        Right _ -> Left "Function already declared in enviroment"

extendVar :: Env -> Id -> Type -> Either TypeError Env
extendVar (sig, c:cs) id tp = do
    let var = lookupVar (sig, [c]) id 
    case var of
        Left _ -> Right (sig, insert id tp c:cs)
        Right _ -> Left $ "Variable " ++ show id ++ " already declared in enviroment"

multExtendVars :: Env -> [Id] -> Type -> Either TypeError Env
multExtendVars env [] tp       = Right env
multExtendVars env _ Type_void = Left "Can not declare variables of type void"
multExtendVars env (id:ids) tp = do
    updEnv <- extendVar env id tp
    multExtendVars updEnv ids tp

lookupFunc :: Env -> Id -> Either TypeError FuncType
lookupFunc (sig, c) id = maybe (Left (show id ++ "function not in environment")) Right (Data.Map.lookup id sig)

lookupVar :: Env -> Id -> Either TypeError Type
lookupVar (_, []) id     = Left $ "Variable " ++ show id ++ " not in environment"
lookupVar (sig, c:cs) id = maybe (lookupVar (sig, cs) id) Right (Data.Map.lookup id c)
 
newBlock :: Env -> Env
newBlock (sig, cs) = (sig, Map.empty:cs) 

exitBlock :: Env -> Env
exitBlock (sig, c:cs) = (sig, cs)

emptyEnv :: Env
emptyEnv = (predefinedFun, [])

predefinedFun = Map.fromList [
    (Id "readInt", ([],Type_int)),
    (Id "printInt", ([Type_int],Type_void)),
    (Id "printDouble", ([Type_double], Type_void)),
    (Id "readDouble", ([],Type_double))
    ]

-------- checking --------

typecheck :: Program -> Either TypeError AnnotatedProgram
typecheck (PDefs [])  = Left "File should contain a main method"
typecheck (PDefs def) = do
    env <- multExtendFun emptyEnv def
    defs <- mapM (checkDef env) def
    Right $ PDefs defs

multExtendFun :: Env -> [Def] -> Either TypeError Env
multExtendFun env []     = do
    main <- lookupFunc env (Id "main")
    case main of 
        ([], Type_int)  -> Right env
        (_, _) -> Left "Main function has to be type int"
multExtendFun env (d:ds) = do
    upEnv <- extendFunc env d
    multExtendFun upEnv ds

checkDef :: Env -> Def -> Either TypeError Def
checkDef env (DFun retTp id args stms) = do
    let updEnv = newBlock env
    updEnv' <- multExtendArgs updEnv args
    checkedStm <- checkStms updEnv' stms retTp
    Right $ DFun retTp id args checkedStm

multExtendArgs :: Env -> [Arg] -> Either TypeError Env
multExtendArgs env []       = Right env
multExtendArgs env ((ADecl Type_void id):_) = Left $ "Variable " ++ show id ++ " wrongly has void type"
multExtendArgs env ((ADecl tp id):args) = do
    updEnv <- extendVar env id tp
    multExtendArgs updEnv args


checkStms :: Env -> [Stm] -> Type -> Either TypeError [Stm]
checkStms env [] typ     = Right []
checkStms env (s:ss) typ = do
    (updEnv, stm) <- checkStm env s typ 
    stms <- checkStms updEnv ss typ
    Right $ stm : stms

checkStm :: Env -> Stm -> Type -> Either TypeError (Env,Stm)
checkStm env stm ownerType = case stm of
    
    SExp exp -> do
        inferede1 <- inferExp env exp 
        Right (env,SExp inferede1) 

    SDecls tDecl ids -> do
                updEnv <- multExtendVars env ids tDecl
                Right (updEnv,SDecls tDecl ids)

    SInit tInit id exp -> do
        updEnv <- extendVar env id tInit
        infered <- inferExp updEnv exp 
        case infered of
            (ETyped inferedExp inferedType) -> 
                if isLarger inferedType tInit
                    then do
                        updEnv' <- extendVar env id inferedType
                        Right (updEnv',SInit tInit id infered)
                else Left "Incorrect type"
            other -> Left $ "language error: untyped expression " ++ show other

    SReturn exp -> do
        let infered = checkExp env exp ownerType
        case infered of 
            Left m -> Left $ "Return type should be " ++ show ownerType
            Right i -> Right (env, SReturn i)
            
    SWhile exp innerStm -> do
        checkedExp <- checkExp env exp Type_bool
        (updEnv,checkedStm) <- checkStm (newBlock env) innerStm ownerType
        Right (exitBlock updEnv, SWhile checkedExp checkedStm)

    SBlock stms -> do
        let updEnv = newBlock env
        checkedStms <- checkStms updEnv stms ownerType
        Right  (env,SBlock checkedStms)

    SIfElse exp if_ else_ -> do
        checkedExp <- checkExp env exp Type_bool
        (_,checkedIf) <- checkStm (newBlock env) if_ ownerType 
        (_,checkedElse) <- checkStm (newBlock env) else_ ownerType
        Right (env, SIfElse checkedExp checkedIf checkedElse)


checkExp :: Env -> Exp -> Type -> Either TypeError Exp
checkExp env exp tp = do
    infered <- inferExp env exp
    case infered of 
        (ETyped inferedExp inferedType) -> 
            if isLarger inferedType tp
                then Right infered
                else Left $ (show exp) ++ " should be of type " ++  show tp ++ " but is of type " ++ show inferedType
        other -> undefined

------------- infer type ------------

inferExp :: Env -> Exp -> Either TypeError Exp
inferExp env (EBool b) = Right $ ETyped (EBool b) Type_bool
inferExp env (EInt b) = Right $ ETyped (EInt b) Type_int
inferExp env (EDouble b) = Right $ ETyped (EDouble b) Type_double
inferExp env (EPost id op) = inferPost env id (EPost id op) 
inferExp env (EPre op id) = inferPost env id (EPre op id) 
inferExp env (EMul e1 op e2) = inferAlgebra env (EMul e1 op e2) e1 e2
inferExp env add@(EAdd e1 op e2) = inferAlgebra env add e1 e2
inferExp env (ECmp e1 op e2) = inferAlgebra env (ECmp e1 op e2) e1 e2
inferExp env (EAnd e1 e2)    = inferLogic env (EAnd e1 e2) e1 e2
inferExp env (EOr e1 e2)    = inferLogic env (EOr e1 e2) e1 e2
inferExp env (EId id) = fmap (ETyped (EId id)) (lookupVar env id)
inferExp env (EApp id args) = do
    (argTps, retTp) <- lookupFunc env id
    inferApp env id argTps args
inferExp env (EAss id exp) = do
    let tpVar = lookupVar env id
    let typedExp = inferExp env exp
    case tpVar of
        Left m -> Left $ show id ++ " never declared \n" ++ m
        Right paramTp -> case typedExp of
            Left m1 -> Left m1
            Right (ETyped exp expTp) -> 
                if isLarger expTp paramTp
                    then Right $ ETyped (EAss id (ETyped exp expTp)) expTp
                    else Left $ "Can not assign type " ++ show expTp ++ " value to variable type " ++ show paramTp

-- infers function application
inferApp :: Env -> Id -> [Type] -> [Exp] -> Either TypeError Exp
inferApp env id [] []         = do
    (_, returnTp) <- lookupFunc env id 
    Right $ ETyped (EApp id []) returnTp
inferApp env id (paramTp:ts) (e:es) = do 
    expTp <- inferExp env e 
    case expTp of 
        eTyped@(ETyped e2 argTp) ->
            if isLarger argTp paramTp
                then do
                    tail <- inferApp env id ts es
                    case tail of 
                        (ETyped (EApp _ e3) t3) -> 
                            Right $ ETyped (EApp id (eTyped:e3)) t3
                        other -> undefined
            else Left $ "Argument " ++ show e ++ " is not of type " ++ show paramTp
        other -> undefined
inferApp env id [] (e:es) = Left $ "Argument " ++ show e ++ " is superfluous"
inferApp env (Id id) (t:ts) [] = Left $ "The function " ++ id ++ " has too few arguments" 


inferPost :: Env -> Id -> Exp -> Either TypeError Exp
inferPost env id identifier = do
    typ <- lookupVar env id
    if typ==Type_int || typ==Type_double then
        case identifier of 
            (EPost eId op) -> Right $ ETyped (EPost eId op) typ 
            (EPre op eId) -> Right $ ETyped (EPre op eId) typ 
    else Left "cannot make numeric operation"

inferAlgebra ::  Env -> Exp -> Exp -> Exp -> Either TypeError Exp
inferAlgebra env identifier exp1 exp2 = do
    inferede1 <- inferExp env exp1
    inferede2 <- inferExp env exp2
    case inferede1 of 
        (ETyped e typ) ->  
            if typ==Type_int || typ == Type_double
            then 
                do 
                    inferede2 <- inferExp env exp2
                    case inferede2 of
                        (ETyped _ typ2) -> do
                            largerTyp <- castType typ typ2
                            case identifier of
                                (EAdd e1 op e2) -> Right $ ETyped (EAdd inferede1 op inferede2) largerTyp
                                (EMul e1 op e2) -> Right $ ETyped (EMul inferede1 op inferede2) largerTyp
                                (ECmp e1 op e2) -> Right $ ETyped (ECmp inferede1 op inferede2) Type_bool --typ
                        other -> undefined
            else 
                if typ == Type_void
                    then Left "Can not perform algebraic operations on void type expressions"
                else do 
                    inferede2 <- inferExp env exp2
                    case identifier of
                        (ECmp e1 OEq e2) -> Right $ ETyped (ECmp inferede1 OEq inferede2) Type_bool
                        (ECmp e1 ONEq e2) -> Right $ ETyped (ECmp inferede1 ONEq inferede2) Type_bool
                        other -> Left $ " Can not perform add/sub/mult/div/lessthan/morethan on boolean type expression " ++ show identifier
        other -> undefined

castType :: Type -> Type -> Either TypeError Type
castType Type_int Type_int = Right Type_int
castType _ Type_bool       = Left $ "bad type"
castType Type_bool _       = Left "bad type"
castType _ Type_void       = Left "Can not cast type void"
castType Type_void _       = Left "Can not cast type void"
castType _ _               = Right Type_double

isLarger :: Type -> Type -> Bool
isLarger Type_int Type_double    = True
isLarger Type_double Type_double = True
isLarger Type_int Type_int       = True
isLarger Type_void Type_void     = True
isLarger Type_bool Type_bool     = True
isLarger _ _                     = False


inferLogic :: Env -> Exp -> Exp -> Exp -> Either TypeError Exp
inferLogic env identifier e1 e2 = do
    inferede1 <- inferExp env e1
    case inferede1 of
        (ETyped e typ) -> 
            if typ==Type_bool then 
                do 
                    inferede2 <- checkExp env e2 typ
                    case identifier of
                        (EAnd e1 e2) -> Right $ ETyped (EAnd inferede1 inferede2) typ
                        (EOr e1 e2) -> Right $ ETyped (EOr inferede1 inferede2) typ
            else Left "the type of the expression is not correct"
        _ -> undefined

