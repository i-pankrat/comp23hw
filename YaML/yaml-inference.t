Show help
  $ ./yaml.exe -help
  yaml -i -d -f <file>
  Compile program from stdin
    -i Infer only the types for the input, do not use the compiler.
    -f Read program from specified file, not from the stdin.
    -d Disable occurrence checking during type checking
    -help  Display this list of options
    --help  Display this list of options
Fixed point combinators
  $ ./yaml.exe -i -f ./tests/combinators.ya
  fix_y: (('a -> 'a) -> 'a)
  fix_z: ((('a -> 'b) -> ('a -> 'b)) -> ('a -> 'b))
Factorial
  $ ./yaml.exe -i -f ./tests/factorial.ya
  fac: (int -> int)
  fac10: int
  fac_tailrec: (int -> int)
  fac_tailrec5: int
  fix_y: (('a -> 'a) -> 'a)
  fix_z: ((('a -> 'b) -> ('a -> 'b)) -> ('a -> 'b))
  fac_open: ((int -> int) -> (int -> int))
  fix_y_fac: (int -> int)
  fix_z_fac: (int -> int)
  fix_y_fac5: int
  fix_z_fac5: int
Fibonacci
  $ ./yaml.exe -i -f ./tests/fibonacci.ya
  fib: (int -> int)
  fib10: int
  fib_tailrec: (int -> int)
  fib10: int
  fib_cps: (int -> int)
  fib_cps10: int
  fix_y: (('a -> 'a) -> 'a)
  fix_z: ((('a -> 'b) -> ('a -> 'b)) -> ('a -> 'b))
  fib_open: ((int -> int) -> (int -> int))
  fib_y: (int -> int)
  fib_z: (int -> int)
  fib_y5: int
  fib_z5: int
Occurs check is disabled
  $ ./yaml.exe -i -d -f ./tests/occurs-check-disable.ya
  fix: (('a -> 'a) -> 'a)
  fac_open: ((int -> int) -> (int -> int))
  fib_open: ((int -> int) -> (int -> int))
  fac: (int -> int)
  fib: (int -> int)
  fac5: int
  fib5: int
