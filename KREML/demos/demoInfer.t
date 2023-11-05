  $ ./demoInfer.exe <<- EOF
  > fun fact n =
  > let fun fact_tail acc n =
  > if n <= 1 
  > then acc
  > else fact_tail (acc * n) (n - 1)
  > in
  > fact_tail 1 n
  > end
  > EOF
  int -> int
