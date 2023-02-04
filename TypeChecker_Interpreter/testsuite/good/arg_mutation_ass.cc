void cdec(int c) {
  c--;
}

int main () {
  int c;
  cdec(c = 0);
  printInt(c);

  return 0;
}

// PDefs [DFun Type_void (Id "cdec") [ADecl Type_int (Id "c")] 
// [SExp (EPost (Id "c") ODec)],DFun Type_int (Id "main") [] [SDecls Type_int [Id "c"],SExp (EApp (Id "cdec") [EAss (Id "c") (EInt 0)]),SExp (EApp (Id "printInt") [EId (Id "c")]),SReturn (EInt 0)]]