Show help
  $ ./yaml.exe -help
  yaml -i -f <file>
  Compile program from stdin
    -i Infer only the types for the input, do not use the compiler.
    -f Read program from specified file, not from the stdin.
    -help  Display this list of options
    --help  Display this list of options
Factorial
  $ ./yaml.exe -i -f ./tests/factorial.ya
  fac: (int -> int)
  fac10: int
  fac_tailrec: (int -> int)
  fac_tailrec5: int
  fact_cps: (int -> int)
  fact_cps10: int
Fibonacci
  $ ./yaml.exe -i -f ./tests/fibonacci.ya
  fib: (int -> int)
  fib10: int
  fib: (int -> int)
  fib5: int
  fib_tailrec: (int -> int)
  fib10: int
  fib_cps: (int -> int)
  fib_cps10: int
