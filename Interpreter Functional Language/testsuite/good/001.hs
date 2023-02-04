max = 600 ;
id x = x ;
twice f x = f (f x) ;
first x y = x ;
second x y = y ;
main = print (first (twice (\x -> x)) 6 7) ; -- result 7


{-
Env {
    functions = fromList [
        (Ident "first",VClos (EVar (Ident "x")) (Env {functions = fromList [], variables = fromList []})),
        (Ident "id",VClos (EVar (Ident "x")) (Env {functions = fromList [], variables = fromList []})),
        (Ident "max",VClos (EInt 600) (Env {functions = fromList [], variables = fromList []})),
        (Ident "second",VClos (EVar (Ident "y")) (Env {functions = fromList [], variables = fromList []})),
        (Ident "twice",VClos (EApp (EVar (Ident "f")) (EApp (EVar (Ident "f")) (EVar (Ident "x")))) (Env {functions = fromList [], variables = fromList []}))], 
        variables = fromList []}
-}

{-
(Env {
    functions = fromList [], variables = fromList []})),
    (Ident "id",VClos (EVar (Ident "x")) (Env {functions = fromList [], variables = fromList []})),
    (Ident "max",VClos (EInt 600) (Env {functions = fromList [], variables = fromList []})),
    (Ident "second",VClos (EVar (Ident "y")) (Env {functions = fromList [], variables = fromList []})),
    (Ident "twice",VClos (EApp (EVar (Ident "f")) (EApp (EVar (Ident "f")) (EVar (Ident "x")))) (Env {functions = fromList [], variables = fromList []}))], variables = fromList []}
-}

{-
Env {
    vars = fromList [], 
    funs = fromList [
        (Ident "first",Closure (EAbs (Ident "x") (EAbs (Ident "y") (EVar (Ident "x"))),
            Env {vars = fromList [], funs = fromList []})),
        (Ident "id",Closure (EAbs (Ident "x") (EVar (Ident "x")),Env {vars = fromList [], funs = fromList []})),
        (Ident "max",Closure (EInt 600,Env {vars = fromList [], funs = fromList []})),
        (Ident "second",Closure (EAbs (Ident "x") (EAbs (Ident "y") (EVar (Ident "y"))),Env {vars = fromList [], funs = fromList []})),
        (Ident "twice",Closure (EAbs (Ident "f") (EAbs (Ident "x") (EApp (EVar (Ident "f")) (EApp (EVar (Ident "f")) (EVar (Ident "x"))))),Env {vars = fromList [], funs = fromList []}))]})
-}

{-
Env {
    functions = fromList [
        (Ident "first",VClos (EAbs (Ident "first") (EAbs (Ident "first") (EVar (Ident "x")))) (Env {functions = fromList [], variables = fromList []})),
        (Ident "id",VClos (EAbs (Ident "id") (EVar (Ident "x"))) (Env {functions = fromList [], variables = fromList []})),
        (Ident "max",VClos (EInt 600) (Env {functions = fromList [], variables = fromList []})),(Ident "second",VClos (EAbs (Ident "second") (EAbs (Ident "second") (EVar (Ident "y")))) (Env {functions = fromList [], variables = fromList []})),(Ident "twice",VClos (EAbs (Ident "twice") (EAbs (Ident "twice") (EApp (EVar (Ident "f")) (EApp (EVar (Ident "f")) (EVar (Ident "x")))))) (Env {functions = fromList [], variables = fromList []}))], variables = fromList [(Ident "second",VClos (EInt 5) (Env {functions = fromList [], variables = fromList []}))]}
-}