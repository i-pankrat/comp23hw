  $ ./parser_factorial_test.exe <<- EOF
  > let rec fac n = n
  > EOF
  RecFunction(fac, [ n ], IfThenElse(BinOp(Eq, Var 'n', Const(CInt 1)), Const(CInt 1), BinOp(Mul, Var 'n', Application(Var 'fac', BinOp(Sub, Var 'n', Const(CInt 1)), [])))

